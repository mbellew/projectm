# Video Foreground-Masking Pipeline

App-side pipeline that turns a camera frame into the RGBA the visualizer samples:
**RGB = image, A = foreground mask.** One pipeline serves both a plain RGB webcam and a
Luxonis OAK-D (RGB + depth); depth is an *optional input/capability*, never a second
pipeline. The finished RGBA is handed to libprojectM via the GPU input-texture API
(`projectm_video_get_input_texture` + `projectm_video_submit_frame_gpu`); the library then
copies it verbatim into the 3D video-history ring (no preset alpha mode applied).

**Status (2026-06-12):** the shared back-end (B1–B4) and the RGB-only priors now live in
**libprojectM** (`Renderer/VideoTexture.cpp` + `VideoPreprocessShaders.hpp`), running inside
`VideoTexture::UpdateGPU` after the prior pass, gated by `AlphaParams::refine` and an
app-global override (`projectm_video_set_mask_mode`). Only the hardware/ML front-end
(capture, depthai, Vision/CoreML inference) stays app-side — it has SDK/platform deps the
back-end does not. `projectm_video_get_input_texture` + `submit_frame_gpu` remain the bypass
for an app that wants to do its own masking and hand in a finished RGBA.

## Design decision

- **One pipeline, two halves:** a *pluggable front-end* that emits a coarse foreground
  prior, and a *shared, signal-agnostic back-end* that refines any prior into a stable,
  soft alpha. Depth is one prior source **and** an optional extra guidance channel into the
  shared stages — not a fork.
- **Seg-primary / depth-refined** (not depth-primary): given the OAK-D Lite's holey passive
  stereo, a person-segmentation prior (works on any RGB) is the stronger default; depth
  *refines* it. Depth-band is the prior only when seg isn't available.
- **Fuse, don't sequence:** depth and color have complementary failure modes (depth fails on
  textureless/low-contrast regions; color fails when subject ≈ background color). The shared
  stages consume whichever signals are present and let each cover the other's blind spots.
- **Webcam-first implementation order:** build the RGB-only path first. It has no hardware or
  depthai-port dependency, is independently shippable, and forces the back-end to be
  source-agnostic from day one. The Luxonis depth-band prior + depth guidance then slot in
  with near-zero rework.

## Front-end: prior-source interface

A *prior source* is a strategy that, each frame, emits a coarse foreground signal. All
sources share one contract so the back-end never branches on origin, and so multiple priors
can later be fused.

```cpp
struct PriorFrame {
    GLuint confTex;     // R8/R16F. P(foreground) in [0,1]. 0.5 == "unknown" (defer to back-end).
    int    width, height;   // MAY be lower than working res; back-end up-samples.
    float  anchorDepth;     // depth sources: subject distance (fg_ema); NaN otherwise.
    bool   valid;           // source is confident this frame.
};

struct FrameInputs {
    GLuint rgbTex;          // full-res working image (e.g. 640x480).
    GLuint depthTex;        // R16 aligned depth, or 0 if no depth.
    GLuint prevRgbTex;      // previous frame (motion / bg-subtract / flow).
    GLuint prevAlphaTex;    // previous output alpha (temporal feedback).
    GLuint segTex;          // host-NN person-seg probability, or 0.
    /* pose keypoints, bbox, timestamps ... */
};

struct Capabilities { bool needsDepth, needsSeg, providesPerson; /* ... */ };

class IPriorSource {
public:
    virtual PriorFrame  Compute(const FrameInputs&) = 0;
    virtual Capabilities Caps() const = 0;
};
```

**Key contract points**

- Output is **3-valued confidence**, not binary: `>hi` = foreground, `<lo` = background, the
  middle band = *unknown*. The back-end derives the trimap from this. Depth-invalid pixels
  emit `0.5` (unknown), **not** `0` — never assert "background" from missing depth.
- Prior may be **low-res** (seg nets are ~256²); resolution is decoupled because the guided
  up-fill (B1) joint-bilateral-upsamples it against full-res RGB.

**Prior sources**

| Source        | Needs        | Emits                                   | Runs where |
|---------------|--------------|-----------------------------------------|------------|
| `PersonSeg`   | host-NN seg  | seg probability (soft, coarse)          | host ANE → GPU |
| `DepthBand`   | depth+anchor | band membership around `fg_ema`         | GPU |
| `BgSubtract`  | prev frames  | appearance vs temporal background model | GPU |
| `Motion`/`MotionDecay` | prev | frame-diff (stylistic; preset-chosen)   | GPU |
| `ChromaKey`   | key color    | key distance (real green screen)        | GPU |
| `Constant`/`None` | —        | all-foreground (webcam no-op)           | GPU |

A **selector** ranks available sources by capability
(`PersonSeg > DepthBand > BgSubtract > Motion > Constant`) and picks the best satisfiable one;
it sets the active-mode capability var presets can read. **Fusion** (weighted combine of two
priors — e.g. depth where color is ambiguous, seg where depth drops out) is a reserved
extension: because all priors share the `[0,1]` contract, fusion is just another pass
emitting a `PriorFrame`.

## Shared back-end: pass list

Fullscreen fragment passes (gl_VertexID triangle, RGBA16F intermediates, `glCopyTexSubImage`
where needed) — the same macOS-GL-4.1-safe style as the existing
`Renderer/VideoPreprocessShaders.hpp`. Runs on the app's own FBO; the **last pass renders
into `projectm_video_get_input_texture()`**, then `submit_frame_gpu`.

| Pass | Name              | In                                   | Out          | Notes |
|------|-------------------|--------------------------------------|--------------|-------|
| B0   | Prior             | `FrameInputs`                        | `coarseConf` | front-end strategy (above) |
| B1   | Guided up-fill    | `coarseConf`, RGB, *depth?*          | `denseAlpha` | joint bilateral / guided filter: upsamples + fills interior holes + snaps to edges. **The workhorse.** |
| B2   | Trimap + matte    | `denseAlpha`, RGB, *depth?*          | `matteAlpha` | derive fg/bg/unknown; refine unknown band. v1 = guided edge refine; **v2 (reserved)** = learned matte (RVM/MODNet) seeded by trimap, budget-gated. |
| B3   | Temporal stabilize| `matteAlpha`, `prevAlpha`            | `stableAlpha`| EMA + optional flow-warped prev for fast motion. Flicker control > per-frame accuracy. |
| B4   | Composite+feather | RGB (or app FX), `stableAlpha`       | **RGBA**     | pack RGB+A, final edge feather/despill. Renders into the projectM input texture. |

**Two kinds of hole, handled differently** (B1 vs B2):
- *Interior depth/seg dropout* → flood via color-(and-depth-)guided fill within the mask's
  connected component (B1).
- *Silhouette/hair/fingers* → matting, **not** flooding: trimap + edge refine (B2). Don't
  flood at edges or you bleed into color-similar background.

## Depth threading (capability gating)

Same passes; depth only adds inputs.

- `hasDepth = false`: B0 = `PersonSeg`/`BgSubtract`/`Motion`; B1/B2 guidance = RGB only.
- `hasDepth = true`:
  - B0 may use `DepthBand` (or fuse it with seg).
  - B1/B2 add depth as a **second guidance channel** — silhouette survives even when
    subject ≈ background *color* (depth discontinuity) and vice-versa.
  - B2 marks **far pixels as definite-background**, tightening the trimap.
  - Enables **additive depth-only effect passes** (depth-correct occlusion, relight from
    depth-normals, depth displacement) — these live *outside* this masking chain, present
    only when `hasDepth`. They do not fork the masker.

## (a) Foreground-depth anchor — `DepthBand` detail

Depth segments *distances*, not people; the anchor says which band is the subject.

- Sample depth at host-NN pose keypoints (or seg-mask centroid); aggregate with a **robust
  estimator** (median / Gaussian-weighted median), **rejecting invalid (0) depth** before
  aggregating. EMA over time → `fg_ema` (keypoints jitter; passive-stereo depth is noisy).
- Band is a **soft smoothstep**, inner/outer, not a hard cut. Band half-width should adapt to
  the spread of keypoint depths (a person is ~0.3 m deep; an **arm toward camera** sits
  outside a tight band and would be amputated).
- Continuous-depth-to-wall (subject near background) is the honest failure case → relies on
  the color cue in B1/B2.

## Compute placement

| Stage                                   | Where |
|-----------------------------------------|-------|
| Stereo depth + spatial/temporal/speckle filters, depth↔RGB align, MJPEG color | OAK VPU (on-device) — USB2 bandwidth win |
| Person-seg / pose                       | **Host ANE** (Vision) — faster than Myriad X, removes the slow cross-stream timestamp-match lag |
| B1–B4 + depth-only effect passes        | Host GPU |
| MJPEG decode, depth-anchor reduction    | Host (GPU/CPU) |

depthai is **v2.x** (OAK-D Lite = RVC2/Myriad X). depthai-core integrated via
FetchContent/submodule into sdl-test-ui (downstream/local; the libprojectM API additions are
the upstream-worthy slice).

## Preset-facing capability vars (planned, not yet implemented)

`video_has_depth`, `video_has_person`, `video_active_mode` — read-only, let a preset branch
in-shader. This is the "with/without Luxonis" switch **without** a second code path or a
fork preset authors must know about.

## Budget

Prior (≤256²) + four ≤VGA fullscreen passes is cheap and must co-exist with the visualizer
render each frame. The only heavy optional piece is B2-v2 (neural matte); gate it on measured
GPU headroom. Set the per-frame budget before deciding whether B2 stops at guided filtering.

## Build order

1. **Webcam path:** `PersonSeg` (Vision/ANE) or `BgSubtract`/`Motion` → B1 → B2(v1) → B3 →
   B4 → `submit_frame_gpu`. Validates the whole back-end with no hardware dependency. Ship it.
2. **Luxonis path:** add `DepthBand` prior + depth guidance into B1/B2 + the depth-only effect
   passes. Same passes, more inputs.
3. **Reserved:** prior fusion; B2-v2 neural matte; the preset `video_*=` preprocessing-shader
   hook (parallel to `comp_*`/`warp_*`).
