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
