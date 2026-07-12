# Person-Mask Performance Plan (RTX 5060 / Ubuntu target)

Architectural review of the foreground-person masking subsystem, with a prioritized
optimization plan for the deployment box (AtomMan G1 Pro, RTX 5060 / Blackwell, Ubuntu,
CUDA 13.x / TensorRT 10 available).

Review date: 2026-07-12. Produced by an independent code read (Claude Fable 5).

> **All timing numbers below are ESTIMATES read off the code, not measurements.**
> Step 1 of the plan is to instrument and confirm them. Do not act on steps 3+ before
> the timers exist — the whole diagnosis could be wrong about which stage dominates.

---

## Feature goals (for context)

A live camera feed becomes a preset-samplable texture. A person-segmentation mask
isolates the performer from the background so presets can treat them differently
(composite over generated visuals, drive warping/painting, gate effects). Related
work in flight: YOLO-pose driving preset-readable `touch_*` built-ins, and the
touch-painting preset. **This is a live performance instrument — mask latency is
immediately visible, and responsiveness is the top-line requirement.**

---

## Finding 1: The bottleneck is the CPU, not the GPU and not the readback

The matte readback (`segMask.cpp:817-824`, bound to `OrtMemTypeCPUOutput` pinned
memory) is a 384×384 float tensor ≈ **590 KB — well under 1 ms**. It is not the
problem. The IoBinding path keeping RVM's recurrent state device-resident
(`segMask.cpp:785-845`) is **already the right design and works**. Leave it alone.

The real cost: **everything around the model runs on the CPU at full camera resolution
(up to 1920×1080 ≈ 2 Mpx), serialized on the single V4L2 capture thread**, every frame:

| # | Stage | Location | Note |
|---|-------|----------|------|
| 1 | YUYV→BGRX / MJPEG decode | `videoCapture_linux.cpp:525-582` | MJPEG goes through **stb_image** — single-threaded, ~15–30 ms for 1080p. `kMaxCaptureArea` allows 1080p (`:211`) and the FPS-first selector may well land there. |
| 2 | BGRA→RGB mirror copy at full res | `segMask.cpp:541-552` | ~2 Mpx |
| 3 | CPU bilinear downscale to 384² (`RgbToChw`) | `segMask.cpp` | Fine — cheap. |
| 4 | **ONNX Run (RVM)** | — | A few ms on the 5060. The *smallest* item in the chain. |
| 5 | Matte upscale + composite to full camera res | `segMask.cpp:951-965` | Per-pixel bilinear `SampleMatte` over 2 Mpx, ~5–10 ms. |
| 6 | `HardenAlpha` full-res pass | `segMask.cpp:977-989` | If enabled. |
| 6b | **Depth gate** | `segMask.cpp:997-1245` | **An entire second model (Depth Anything V2) run synchronously every frame**, plus CPU connected components. If enabled. |
| 7 | `VideoTexture::ConvertAndDownscale` | `VideoTexture.cpp:441-502` | A **second** full-res CPU pass, box-filtering 2 Mpx back down to ~852×480. **The full-res composite from step 5 is immediately thrown away** — the library only ever sees ~0.4 Mpx. |
| 8 | Pose re-converts the same BGRA frame to RGB at full res | `poseTracker.cpp:180-193` | **Duplicates step 2**, then letterboxes and runs a second model. |
| 9 | Alpha-weighted centroid | `pmSDL.cpp:352-366` | Full 2 Mpx double-precision loop for a number that would be **identical** computed from the 384² matte pre-upscale (147 K px, ~14× cheaper). |

**Estimated total: 25–60 ms of CPU per frame on one thread**, versus low-single-digit ms
of GPU inference. Mask lag = camera exposure + V4L2 queue + all of the above.

## Finding 2: V4L2 latency bug (independent of throughput)

The capture loop (`videoCapture_linux.cpp:495-593`) requests 4 buffers (`:430`) and
dequeues **one buffer per iteration, FIFO, with no drain-to-newest**. When the callback
is slower than the camera, stale frames are not dropped — they are processed, so the
pipeline runs **permanently up to 4 frames (~130 ms @ 30 fps) behind**. For a live
instrument this alone is visible, regardless of throughput fixes.

---

## Decisions on the open questions

### Execution provider — CUDA is right; **defer TensorRT**

Both models are tiny relative to a 5060. The GPU is not the bottleneck, so the TRT EP's
~2× kernel-level win buys almost nothing end-to-end, while adding engine-build stalls,
version pinning, and a dynamic-shape problem: **RVM's recurrent tensors start as
1×1×1×1 zeros (`segMask.cpp:336-342`) and change shape on frame 2**, forcing TRT
re-optimization unless the zero states are pre-shaped and `PROJECTM_SEG_SIZE` is pinned.
`LINUX_SETUP.md:258` already notes TRT is unwired. Revisit only if measurement shows
inference dominating (e.g. after moving to RVM-ResNet50 @512).

**Cheap GPU win instead: offline FP16 export of the RVM ONNX** (`onnxconverter_common`).
RVM is FP16-safe; the CUDA EP will use tensor cores. Roughly halves inference, no
runtime complexity.

### Model — **keep RVM**

Right model for live single-performer matting: recurrent → temporally stable, soft alpha,
real-time by design. MODNet / U²-Net flicker (stateless). BiRefNet-class isn't 60 fps.
MediaPipe selfie seg is coarser. On the 5060 there is headroom the Mac lacks — try
**rvm_resnet50 @ 512, FP16** for visibly better edges, but **only after the CPU path is
fixed**, or it just widens the full-res CPU loops.

**Bug:** the recurrent state is zeroed in `Load()` and **never reset**. After an occlusion,
lighting snap, or camera glitch, stale state smears. Needs a reset hook (re-zero
`m_impl->recurrent`) on capture restart / long frame gap.

### CUDA↔GL interop — **skip**

Would eliminate copies totalling ~1–2 MB/frame at model resolution — **well under 1 ms**.
Costs: CUDA toolkit in the app build; the capture thread has **no GL context**, so all
interop must hop to the GL thread or use shared contexts + sync objects; plus ORT
device-input binding plumbing. Poor impact ÷ effort.

### The move that *is* worth it — **stop compositing on the CPU**

Architectural, not interop. The library already has the receiving end built:
`InputTextureId()` + `SubmitFrameGPU()` (`VideoTexture.hpp:96-105`), and the refinement
back-end (B1 guided fill, `VideoPreprocessShaders.hpp:191-220`) exists precisely to snap
a coarse matte to color edges on the GPU.

The seg callback should shrink to: **decode camera → downscale once → run RVM → hand the
library the RGB frame + the raw 384² matte as a small texture**, and let a GL pass do
upscale / composite / harden at texture resolution.

This removes stages 5, 6, and 7 above entirely, with **no CUDA interop**. Per
`MASK_LAYERS.md:110` the guided-fill passes are currently **bypassed for the seg channel** —
so this fixes edge quality and speed in one change.

### GPU contention — **a non-issue; don't build scheduling machinery**

RVM@384 + YOLO-pose@640 + 480p preprocess is a small fraction of a 5060 frame. Each ORT
session has its own stream; the driver arbitrates against GL fine.

**CPU serialization is the real issue** — seg, pose, depth gate, and centroid all run
sequentially in one callback (`pmSDL.cpp:326-462`). Fix with cadence, not threads:
- **Pose every 2nd–3rd frame.** The pose→touch bridge already smooths; 20–30 Hz keypoints
  are indistinguishable for painting.
- **Depth gate every 4th–5th frame**, reusing `cellWeight` between runs. Spectators don't
  teleport. It is currently a full extra model, synchronously, every frame.
- Optionally move pose to its own thread consuming the shared RGB buffer — **only if the
  timers say it's needed.**

### Do **not** merge seg + pose into one model

No off-the-shelf network does recurrent matting + keypoints. Custom training is its own
project, and matte quality would suffer. Two small specialized models at different
cadences is the right shape.

---

## Mask quality (not just speed)

- **Edges** are currently limited by CPU bilinear upscale of a 384 matte. The library's
  B1/B2 guided-fill + matte passes are the right tool and are bypassed for the seg channel
  (`MASK_LAYERS.md:110`). Moving compositing GPU-side fixes both at once. Keep `HardenAlpha`
  as a knob (defaults off) but do it in the shader.
- **Temporal stability:** RVM's recurrence is the main flicker defense and is correctly
  preserved on-device. The B3 temporal EMA (rate 0.8, `VideoTexture.cpp:295`) is a sensible
  second stage. U²-Net's **per-frame min/max stretch** (`segMask.cpp:934-949`) *guarantees*
  flicker — that family is a dead end for live use.
- **Partial / occluded bodies:** the depth-gate salience anchoring (area × centrality,
  `segMask.cpp:1129-1166`) is thoughtful but is the most expensive and most fragile piece.
  **Pose-based gating can likely replace the depth model entirely** — pose boxes give
  per-person identity nearly free, and "keep components overlapping a tracked skeleton" is
  cheaper and more robust than ranking monocular depth. (This matches the already-parked
  idea in the project notes.)

---

## Prioritized plan (impact ÷ effort)

1. **Instrument first** *(hours)* — per-stage timers in the seg callback (pose already has
   one, `poseTracker.cpp:214-230`; seg has none) + log the negotiated camera mode.
   **Everything below should be confirmed by these numbers.**
2. **Drain the V4L2 queue to newest** *(~20 lines, `videoCapture_linux.cpp`)* — DQBUF until
   EAGAIN, decode only the newest, requeue the rest. Direct latency cut, zero risk.
3. **Kill full-res CPU work** *(1–2 days, all downstream)* —
   - downscale once early to ~texture res and run the whole chain there;
   - compute the centroid from the 384² matte pre-upscale;
   - pass seg's `rgbBuf` to pose instead of re-converting;
   - drop `kMaxCaptureArea` to ~720p (the output texture is 480p — 1080p buys nothing but
     decode time).
4. **Cadence-decouple pose and the depth gate** *(hours)*.
5. **FP16 RVM export** on the Linux box *(hours)*.
6. **GPU composite via the existing `SubmitFrameGPU` path** *(a few days)* — submit RGB +
   small matte texture; upscale / harden / refine in GL. Biggest structural cleanup;
   unlocks higher matte quality for free.
7. **RVM-ResNet50 @512 FP16** on the 5060 once GPU-composited; add recurrent-state reset on
   discontinuity.

### Skip / defer
- **TensorRT EP** — until inference measurably dominates.
- **CUDA↔GL interop** — sub-ms savings, high complexity.
- **Merged seg+pose model** — dead end.
- **U²-Net family support** — per-frame normalization flickers; candidate for deletion along
  with its filename-sniffing detection.
- **stb_image MJPEG decode** — *if* measurements show MJPEG in play, swap to libjpeg-turbo
  (3–5×). Only then.

---

## Code health

`segMask.cpp` (~1250 lines) carries **four model families + secondary-model combine + depth
gate**, while the live path exercises only RVM (+ optional depth):
- **U²-Net is dead weight** and actively harmful (guaranteed flicker).
- The **secondary-multiply path** doubles inference and looks superseded by the gate/depth
  mechanisms.

The library side (`VideoTexture`) is in good shape — clean app/library boundary,
vendor-neutral, preset-controlled — and needs nothing for this effort **beyond optionally
accepting a separate small matte texture alongside the GPU-submit path**. This keeps the
upstream-contribution surface clean (per the upstream strategy: `sdl-test-ui/` is
local/downstream, `libprojectM/` is the likely upstream contribution).

---

# MEASURED — first pass (2026-07-12)

Step 1 of the plan (instrument) is done, and the numbers **contradict the central diagnosis on
this machine** — for a reason nobody predicted.

> **Machine: MacBook Air, macOS, CoreML/ANE. This is NOT the deployment box.** The absolute
> numbers will not transfer to the RTX 5060 (CUDA). What does transfer: the *relative* shape of
> the CPU-side full-res loops, and the model-configuration finding below.

Method: `PROJECTM_SEG_PERF_EVERY=N` (new; default 0 = off) logs a `[SegPerf]` line every Nth
captured frame with per-stage cost. Everything in the capture callback is serial on one thread,
so the total **is** the mask-latency budget.

## Finding A (the big one): the live seg model was U²-Net, not RVM

The analysis above assumes RVM. It wasn't running. `~/.projectM/config.inp` had

    Video Seg Model = .../u2net_human_seg.onnx     # 168 MB

as the **primary** model, with the RVM line commented out — even though the comment directly
above it documents the intent as *"Run RVM as the primary and gate it by u2net"*. The active
lines were simply inverted.

Same run, only the model changed (1920×1080 capture, no depth gate, harden off):

| Stage | U²-Net (as configured) | RVM (as intended) |
|---|---|---|
| seg **inference** | **180.6 ms** | **26.0 ms** |
| seg rgb build (full-res) | 1.5 | 1.6 |
| seg composite (full-res) | 4.5 | 4.5 |
| submit | 1.9 | 1.7 |
| pose (YOLO-pose) | 18.8 | 19–27 |
| centroid (full-res) | 2.1 | 2.1 |
| **TOTAL capture frame** | **209 ms** | **~57 ms** |

**~3.5× end-to-end, from one config line.** Confirmed visibly faster in use. This makes the
"U²-Net is dead weight" call in *Code health* the highest-value item in the whole document — it
was not merely flicker-prone, it **was** the bottleneck.

Corollary: the *secondary-multiply* path (`Video Seg Model 2 = u2net`) would add U²-Net's ~180 ms
straight back. The "gate RVM by u2net" idea is unaffordable at U²-Net's cost, whatever its
quality merits.

## Finding B: with RVM, the models dominate — the CPU does not (on this box)

Finding 1 estimated **25–60 ms of CPU** per frame around the model. Measured: **~8 ms**.

- seg inference 26 ms (45%) + pose 19–27 ms (~40%) = **~85% of the frame is the two models**
- all full-res CPU work — rgb 1.6 + composite 4.5 + centroid 2.1 = **8.2 ms (14%)**

So plan steps **3 and 6** (kill full-res CPU work; GPU composite) are chasing ~8 ms here, not
25–60 ms. **This is expected to flip on the 5060**: CUDA should put RVM inference in the low
single-digit ms, at which point the same ~8 ms of CPU *does* dominate and steps 3/6 become the
right move. Re-measure there before acting — which is what the plan said.

Also confirmed: **capture negotiates 1920×1080** while the video texture is only ~852×480. Every
full-res CPU loop is run on 2 Mpx that is immediately thrown away. Capping `kMaxCaptureArea` at
720p should cut that ~8 ms to ~3 ms for free (Finding 1, stage 7 / plan step 3).

## Landed in this pass

1. **Instrumentation** — `SegTimings` (rgb / infer / composite / harden / depth / total) exposed
   via `SegMasker::LastTimings()`, plus an end-to-end `[SegPerf]` line in the capture callback
   (`PROJECTM_SEG_PERF_EVERY`, default off). This is what goes to the AtomMan to get the numbers
   that actually decide steps 3+.
2. **Removed the duplicated full-res RGB conversion** (Finding 1, stage 8) — `PoseTracker` was
   re-converting the identical BGRA frame to the identical RGB buffer that `SegMasker::Process`
   had just built. `PoseTracker::ProcessRgb()` now consumes `SegMasker::RgbFrame()` directly.
   ~1.6 ms/frame at 1080p, and it stops the pipeline running the same 2 Mpx loop twice.

Deliberately **not** done yet: the centroid-from-low-res-matte change (stage 9). The claim that
it is "identical" computed from the 384² matte holds only with the depth gate and `HardenAlpha`
off — both mutate the alpha the centroid is derived from. A stride-subsample of the *final* alpha
is the safe version; it is worth ~2 ms and can wait for real numbers.

## Next

- Re-run `PROJECTM_SEG_PERF_EVERY` on the AtomMan (RTX 5060 / CUDA) and re-derive the plan from
  those numbers. Expect inference to collapse and the CPU share to rise.
- Fix the model config (RVM primary).
- Then, in order of measured payoff: cap capture resolution → pose/depth cadence → GPU composite.

---

# MEASURED — second pass: the deployment box (2026-07-12)

Same day, on the **AtomMan G1 Pro / RTX 5060 / CUDA** — the machine the plan was written for. Config
as deployed: RVM primary, Seg Quality 3 (512), depth gate on, YOLO-pose on.

**Both of the first pass's predictions for this box were wrong.**

1. *"Expect inference to collapse."* It did not. RVM@512 on CUDA is **10.3 ms**, not low-single-digit.
2. *"Expect the CPU share to rise, making steps 3/6 the right move."* It did not — because the camera
   negotiates **640x480**, not 1080p. All the full-res CPU work totals ~5 ms.

Baseline: **~32 ms/frame** — inference 10.3 + depth gate 9.4 + pose 6.3 + composite 2.1 + submit 2.1
+ rgb/harden/centroid 1.0. The models are ~82% of the frame. Against a 33 ms camera interval, that is
**~1 ms of headroom**.

## Finding C: display *aspect* silently picks the camera mode — and the whole cost curve with it

`FPS = 30` in config means every mode this camera offers meets the FPS floor, so the selector falls
through to **display aspect** (`videoCapture_linux.cpp` `selectCaptureFormat`). The 1024x768 projector
is 4:3, and the only 4:3 modes offered are 320x240 and 640x480 — hence 640x480, hence a cheap frame.

Force a 16:9 aspect (a venue TV) and the very same code negotiates **1920x1080 MJPEG**: 60 ms/frame,
dropping a stale frame nearly every iteration — roughly half the camera's rate, permanently behind.
**Finding 1 of the original review is correct after all; it just needs a 16:9 display to show up.**
The full-res CPU stages scale with capture megapixels, the two models do not:

| Stage | 640x480 (4:3) | 1280x720 | 1920x1080 (16:9) |
|---|---|---|---|
| seg inference | 10.3 | 10.5 | 10.6 |
| pose | 6.3 | 5.6 | 5.9 |
| depth gate | 9.4 | 12.1 | 20.1 |
| composite | 2.1 | 6.1 | 13.8 |
| harden / rgb / centroid / submit | 3.1 | 5.4 | 9.6 |
| **TOTAL** | **~32** | **~40** | **~60** |

Note the video *history texture is 852x480 regardless of display* (`setup.cpp`: short side pinned at
480; only the long side follows the aspect), and the seg model is <=512px. Capturing 1080p to feed
that is pure waste. This is independent of the [1080, 2160] internal-render floor in
`projectMSDL::applyRenderSize()` — three different sizes, often conflated:
**main/feedback texture** (window x supersample, floored at 1080) vs **video history texture**
(852x480, fixed) vs **camera capture** (negotiated, now capped).

## Finding D: the depth gate is 7.5 ms of model + 6 ms/megapixel of CPU apply

Fitting the three resolutions above: depth cost ~= **7.5 ms fixed + ~6 ms/Mpx**. The fixed part is
Depth Anything V2; the per-megapixel part was **step 7 alone** — a full-res bilinear fetch per pixel
to multiply the coarse keep-grid into alpha. The decision-making (grid, connected components, anchor)
is O(grid) and free. So "the depth gate is expensive" was half a depth problem and half the *same*
full-res compositing problem.

## Landed in this pass

1. **Drain V4L2 to the newest frame** (`588c28e2`). Not a throughput fix — a latency fix. The 496 ms
   startup frame (CUDA warmup) buries 2-3 buffers; the old one-DQBUF-per-iteration loop would work
   through them FIFO and run ~100 ms behind *for the rest of the session*. Now stale buffers are
   requeued and only the newest is decoded, so a hitch costs one frame, not a permanent lag.
   `PROJECTM_VIDEO_DROP_DEBUG=1` logs the drops (without it the fix is invisible).
2. **Cap capture at 720p** (`588c28e2`). 16:9 only: **60.2 -> 39.7 ms**. No quality cost — 720p still
   exceeds the 852x480 texture and the 512px model in every dimension. 4:3 unaffected.
3. **`PROJECTM_DISPLAY_ASPECT`** (`0bdc091a`). Negotiate the camera mode a *different* display would
   pick, from this desk. Finding C is invisible without it.
4. **Apply the alpha gate on the GPU** (`d012a60e`). New library API
   (`projectm_video_submit_alpha_gate`): the app hands over the coarse keep-grid and the preprocess
   shader multiplies it into the matte — in **both** the processed alpha and the mask buffer's seg
   channel (seg reads the *input* alpha, so gating only the former would leave presets sampling
   `mask.r` still seeing the gated-out people). Deletes step 7's full-res pass:
   **4:3 32.0 -> 29.5 ms; 16:9 39.7 -> 34.0 ms.**
   The two CPU consumers of the matte — the alpha-weighted centroid and the pose->touch wrist
   confidence — no longer get gating for free, and now weight themselves via `SegMasker::SampleGate`.
   *Miss this and a gated-out spectator's wrist silently keeps full confidence and can drive the
   touch bridge.* The centroid also strides by 4 (a centroid is an integral; 1/16th the work).

Verified: a forced-constant gate scales GPU alpha exactly linearly (1.0 -> mean 0.145, 0.5 -> 0.075,
0.0 -> 0.000), and screenshots show a clean silhouette at gate=1, an empty frame at gate=0.

## Where the frame stands

| | original | + drain/cap | + GPU gate | + 1:1 submit | (+ depth 196, not applied) |
|---|---|---|---|---|---|
| 4:3 projector | 32.0 | 32.0 | 29.5 | **26.8** | 22.8 |
| 16:9 TV | 60.2 | 39.7 | 34.0 | **33.5** | 30.5 |

The 16:9 case went from ~half the camera rate to roughly keeping pace. It is still marginally over
the 33 ms interval; depth-input size (below) is what would close that.

## Open: depth input size, to be decided with the pose-gating story

`PROJECTM_SEG_DEPTH_SIZE` (default 392 long side) is now the largest lever left. Measured at 640x480:

| long side | depth cost | refClose (the gate's reference) |
|---|---|---|
| 392 (default) | 7.2 ms | 0.32 |
| 308 | 5.0 ms | 0.35 |
| 196 | 3.2 ms | 0.37 |
| 140 | 2.7 ms | **0.16 — keep threshold goes negative; the gate silently stops gating** |

**Not defaulted, deliberately.** The sweep had only *one person* in frame, so it shows cost and shows
the depth statistics stay stable down to 196 — but it never exercises the thing the gate exists for:
ranking a subject against a spectator. That needs a two-person scene.

More to the point, "how much depth resolution does the gate need?" is really a question about *what
the gate is for*, which is entangled with the parked idea of **using pose to inform the alpha gate**
(pose boxes give per-person identity nearly free, and we already pay for YOLO-pose every frame). Decide
the two together rather than tuning a number that a pose-informed gate may make moot.

## Finding E: 60 fps is not reachable — the camera caps at 30

The WyreStorm FOCUS 210 offers **no mode above 30 fps at any resolution** (30/15/10 only; verified with
`v4l2-ctl --list-formats-ext`). So a 60 fps mask is impossible on this hardware no matter how fast the
pipeline gets, and "run at 60, target 30" was never available. Compute headroom buys two things
instead: resilience (a heavy preset or a thermal dip stops meaning dropped frames) and **latency** —
every ms off the callback is a ms less lag between the performer moving and the mask reacting.
Genuinely fresher masks need a 60 fps camera; that is a purchase decision, not a code one.

## Finding F: half of "seg inference" is CPU preprocessing

`timings.inferMs` spans preprocess **and** the ONNX Run together, which hid the split. Measured:

| Seg Quality | CPU preprocess (`RgbToChw`) | ONNX Run + matte readback |
|---|---|---|
| 3 (512x512) | 2.8 ms | **8.6 ms** |
| 2 (384x384) | 1.6 ms | 4.3 ms |

An 8.6 ms Run for RVM-MobileNetV3 on a 5060 is implausibly slow for the arithmetic involved — that
smells like copy/launch overhead, not compute. Relevant to two decisions: FP16 attacks the 8.6 ms
half (and is therefore worth *more* than the original doc credited), and any "parallelize the models"
design depends on that time being GPU wait rather than CPU-side ORT overhead. **Profile before
committing to a threading design** — if it's ORT CPU overhead it will not overlap at all.

## Finding G: on the projector, capture and texture sizes ALREADY match — and the resampler doesn't know

A 1024x768 (4:3) display gives `videoTexW = 480 * 1024/768 = 640`, so the history texture is
**640x480** — exactly the negotiated capture size. `VideoTexture::ConvertAndDownscale` is doing a 1:1
pass with no resampling at all. This is a big part of why the projector numbers came in so far under
the original review's estimate.

But the resampler still paid as if it were resampling: at 1:1 it ran the general box-filter loop,
computing `sx0/sx1/sy0/sy1` with four integer divides per destination pixel, switching on pixel
format *inside* the sample loop, accumulating four sums and dividing by a `count` that is always 1.

**FIXED (`841f94a5`): `submit` 2.1 ms -> 0.1 ms** — a 1:1 fast path (plain format conversion, format
switch hoisted out of the loop, straight `memcpy` for RGBA). 21x, no quality cost, and it is in the
library, so any app whose camera matches its texture benefits. The 16:9 path is untouched (2.7 ms) --
it still needs a real downscale. Projector total: **29.5 -> 26.8 ms**. Verified: both the RGBA (seg)
and BGRX (raw capture) fast paths render correct colors -- a channel swap here would be silent.

Matching the sizes on a 16:9 display is not an option and not desirable:
- the texture would be 853x480; the camera's 16:9 modes are 640x360 (**below** the texture -- it would
  upscale and lose real detail) and 1280x720. There is no 853x480 mode.
- growing the texture to match the camera instead runs into the history ring: it is **120 slices**, so
  853x480 is already ~196 MB of VRAM and 1280x720 would be ~442 MB. The texture is small on purpose.

**The fix is not matching sizes — it is compositing at texture resolution.** Today: composite the matte
into a full-capture-res RGBA frame (6.2 ms @720p), harden it (1.1), then box-filter the whole thing
down to 853x480 (2.7) -- discarding the full-res image just built. The composite already samples the
matte bilinearly per pixel; it can sample the RGB *and* the matte straight into a **texture-resolution**
buffer in one pass and submit that with no downscale step. ~10 ms -> ~3 ms at 720p; ~4.6 -> ~2.5 on the
projector.

The trap to avoid: do **not** downscale the camera frame early to texture resolution. The models want
*more* pixels than the texture (seg 512, pose 640; the texture's short side is 480), so they would be
reading upsampled pixels. Keep building the full-res RGB (0.8 ms) and let the models sample it as they
do now — just stop materializing a full-res *composited* frame whose only purpose is to be shrunk.

## Finding H: the ORT Memcpy node is a red herring (and EP placement is a STATIC analysis)

`PROJECTM_ONNX_DUMP=<dir>` (`7172ff34`) writes each model's post-optimization graph and logs ORT's
per-node EP placement. ORT decides placement when it **builds** the session, so this needs no
inference run and no profiler:

| model | CUDA nodes | CPU nodes | Memcpy inserted |
|---|---|---|---|
| RVM (seg) | 295 | 9 (Slice x7, Concat x2) | **1** |
| Depth Anything V2 | 449 | 128 (Concat/Unsqueeze/Gather) | **0** |

Depth's 128 CPU nodes are int64 *shape arithmetic* — scalar bookkeeping ORT keeps on CPU on purpose.
No data copies. Not a problem.

The RVM Memcpy traces (via `onnx` in Python) to exactly one tensor:
`388 = Concat([1,1], downsample_ratio, downsample_ratio)` — the **scales input to `Resize_3`**. CUDA's
Resize kernel wants its scales in host memory, and `downsample_ratio` is a graph *input*, so it cannot
be constant-folded: ORT computes it on the GPU and copies it back **every frame**.

Folding it to a constant `[1,1,1,1]` works (we always pass ratio 1.0 — we downscale to the seg size
ourselves). Keep `downsample_ratio` as a graph input or **RVM family detection breaks** and the model
loads as MODNet, unbound recurrent inputs and all (`Missing Input: r4i`).

**Result: Memcpy 1 -> 0, matte unchanged, and `infer` unchanged at 10.3 ms. No win.**

Because the copy is **four floats — 16 bytes**. It is not a bandwidth cost at all; its only cost is
that it *blocks CUDA Graph capture* (exactly what ORT's warning says) — and **we never enabled CUDA
Graphs**, so removing the blocker buys nothing by itself. Cashing it in means enabling CUDA Graphs,
which has its own blocker: the IoBinding swaps the recurrent-state buffers every frame and CUDA Graphs
require stable device addresses.

Lesson: the mem->vmem wins so far (full-res composite, the 1:1 box filter) were about **volume**. This
one is about **synchronization**. Don't assume a copy is expensive because it is a copy — 30 minutes of
static analysis said so before a day was spent on CUDA Graphs expecting a payoff.

## The headroom menu (from 29.5 ms on the projector)

Budget today: RVM Run 8.6 | depth 7.3 | pose 6.3 | RVM preprocess 2.8 | composite 2.1 | submit 2.1 |
rgb+harden+centroid 0.9. **Three models are 22 of the 29.5 ms, and they run strictly serially on one
thread.** 40 fps of capacity = 25 ms (cut 4.5); 50 fps = 20 ms (cut 9.5).

Costs nothing visible:
- **FP16 RVM export** (~-4 ms). RVM is FP16-safe; the 5060's tensor cores are idle. Offline export.
- ~~1:1 submit fast path~~ **DONE** (`841f94a5`, -2.0 ms). **Composite at texture res** is the rest of
  Finding G and is still open (~-2 ms projector, ~-7 ms @720p).
- **Run the three models concurrently** (potentially -8 to -10 ms) — the only lever that reaches 50 fps
  with no quality given up, *if* Finding F's 8.6 ms is GPU wait. Profile first.

Costs something real:
- **Seg Quality 3 -> 2** (-4.4 ms): gives up matte crispness. Note FP16 @ Q3 lands near FP32 @ Q2 while
  keeping the sharper matte — prefer FP16 if it works.
- **Depth 392 -> 196** (-4.0 ms): gives up depth ranking fidelity (untested vs a spectator).
- **Pose every 2nd frame** (-3.1 ms): gives up 15 Hz keypoints. It drives the painting preset — hold
  this back longest.

Recommended package to ~40 fps giving up nothing visible: **FP16 + composite-at-texture-res -> ~23.5 ms.**
Cheap route to ~18 ms exists (Q2 + depth 196 + pose cadence) but spends matte crispness, depth fidelity
*and* pose rate — three of the four things the feature exists to do. Don't.

## Re-ranked plan

1. **Pose-informed alpha gate + depth input size**, together (above). Depth stays — it looks effective
   and pose/depth are likely complementary, not either/or.
2. **Composite at texture resolution** (Finding G; the 1:1 `ConvertAndDownscale` fast path is done). No
   quality cost; the gate plumbing (`projectm_video_submit_alpha_gate`) is the same road if it goes to
   the GPU.
3. **FP16 RVM export** (Finding F). Still the best lever on the 8.6 ms Run — for **bandwidth** reasons
   (depthwise convs at 512x512 stream large activations), not copies. Finding H ruled the copies out.
4. **Profile the 8.6 ms Run**, then decide on running the three models concurrently. If CUDA Graphs are
   attempted, note Finding H: the Memcpy blocker is removable offline, but the recurrent-state
   IoBinding swap (unstable device addresses) is the harder one.
5. **Cadence-decouple pose** (~6 ms). Note the depth gate's own cadence is a *worse* idea than it looks:
   the keep mask is ANDed into the subject's own alpha, so a stale grid clips a fast-moving limb
   (~130 ms of staleness moves a hand well past the 1-cell grow margin). If cadence is used, dilate
   `grow` with staleness or union the grid over recent frames.
6. **libjpeg-turbo** — only on a 16:9 display, where the camera hands us MJPEG (its YUYV modes top out
   at 640x480) and the stb_image decode sits on the capture thread, outside every number above.
