# Pose-Driven Touch + Preset-Controllable Touch — Spec

Status: design (2026-07-09). Two separable features that compose:

1. **Preset-controllable touch** — expose the touch event to preset code as
   readable built-ins (`touch_on`, `touch_x`, `touch_y`, `touch_pressure`) plus a
   preset-writable opt-out (`touch_default`). Library-side, mirrors the existing
   `seg_*` and `beat_*` per-frame inputs.
2. **Pose-driven touch** — a host-side bridge that turns body-pose keypoints
   (hands) into a single arbitrated touch stream, with per-hand activation gating
   and sticky hand-selection. Reuses the salience/anchor pattern already built for
   the seg depth gate.

Either can ship without the other. Feature 1 is useful immediately (mouse/touch,
or any external driver). Feature 2 is the motivating use case (hands drive the
visuals) and depends on feature 1's variable surface plus a pose backend.

---

## Background: current state

- The touch C API exists and is fully specced
  ([`touch.h`](src/api/include/projectM-4/touch.h)): `projectm_touch`,
  `projectm_touch_drag`, `projectm_touch_destroy`, `projectm_touch_destroy_all`.
- The library implementation is a **no-op stub**:
  [`ProjectM::Touch`](src/libprojectM/ProjectM.cpp#L583) and friends are empty
  (`// UNIMPLEMENTED`), parameter names stripped.
- The SDL test UI already calls `projectm_touch` from the mouse handler
  ([`pmSDL.cpp:830`](src/sdl-test-ui/pmSDL.cpp#L830),
  [`projectMSDL::touch`](src/sdl-test-ui/pmSDL.cpp#L882)), gated on
  `PROJECTM_TOUCH_ENABLED`. So there is already a live caller producing (x, y,
  pressure) — today it goes nowhere.

So "touch" is a fully-wired *input path* with a dead *destination*. This spec
redefines the destination: instead of (only) drawing a hard-coded waveform, the
touch event becomes preset-readable state, and the built-in waveform becomes an
opt-in default.

---

## Feature 1 — Preset-controllable touch

### Variable surface (engine → preset, read-only inputs)

Exposed in the per-frame context, alongside `seg_*` / `beat_*`:

| Var              | Meaning                                                        |
|------------------|---------------------------------------------------------------|
| `touch_on`       | 1.0 while a touch is active, else 0.0                          |
| `touch_x`        | Touch X, [0,1] left→right (match `seg_cx` convention)          |
| `touch_y`        | Touch Y, [0,1] bottom→top (match `seg_cy` convention)          |
| `touch_pressure` | Pressure, [0,1] (0 if the source has no pressure axis)         |
| `touch_vx`       | X velocity, screen-fractions/sec (optional, mirrors `seg_vx`)  |
| `touch_vy`       | Y velocity, screen-fractions/sec (optional)                   |

Coordinate convention **must** match `seg_cx/seg_cy` exactly (Y is bottom→top) so
presets can treat touch and seg centroid interchangeably.

### Opt-out (preset → engine, writable)

| Var             | Meaning                                                              |
|-----------------|---------------------------------------------------------------------|
| `touch_default` | Reset to **1.0** each frame. If still non-zero after per-frame runs *and* `touch_on > 0.5`, the engine renders the built-in touch waveform at `touch_x/touch_y`. A preset sets `touch_default = 0` to suppress the built-in and handle touch itself. |

This mirrors the existing preset-writable
[`preset_complete`](src/libprojectM/MilkdropPreset/PerFrameContext.hpp#L97),
which is reseeded per frame and read back after execution. Backward compatible:
presets that never mention touch get `touch_default = 1` and the stock waveform.

### Wiring (three touch points, identical to `seg_*`)

1. **Storage** — add fields to
   [`RenderContext`](src/libprojectM/Renderer/RenderContext.hpp#L58) next to the
   `seg*` block:
   ```cpp
   float touchOn{0.0f};       //!< 1.0 while a touch is active (touch_on).
   float touchX{0.5f};        //!< Touch X, [0,1] left to right (touch_x).
   float touchY{0.5f};        //!< Touch Y, [0,1] bottom to top (touch_y).
   float touchPressure{0.0f}; //!< Touch pressure, [0,1] (touch_pressure).
   float touchVx{0.0f};       //!< Touch X velocity, frac/sec (touch_vx).
   float touchVy{0.0f};       //!< Touch Y velocity, frac/sec (touch_vy).
   ```
2. **Declare + register** — add `PRJM_EVAL_F*` members in
   [`PerFrameContext.hpp`](src/libprojectM/MilkdropPreset/PerFrameContext.hpp#L91)
   (seg block) and `REG_VAR(touch_x)` etc. in
   [`PerFrameContext.cpp:59`](src/libprojectM/MilkdropPreset/PerFrameContext.cpp#L59).
   Add `touch_default` next to `preset_complete`.
3. **Reseed per frame** — in
   [`LoadStateVariables`](src/libprojectM/MilkdropPreset/PerFrameContext.cpp#L211):
   ```cpp
   *touch_on       = state.renderContext.touchOn;
   *touch_x        = state.renderContext.touchX;
   *touch_y        = state.renderContext.touchY;
   *touch_pressure = state.renderContext.touchPressure;
   *touch_default  = 1.0;   // reset each frame; preset may zero it
   ```
   After `ExecutePerFrameCode()`, read back `*touch_default`; if `>= 0.5` and
   `touchOn > 0.5`, render the built-in waveform.

### `ProjectM::Touch` implementation

`ProjectM::Touch/TouchDrag/TouchDestroy` stop being no-ops. Minimal version: they
write the current touch point into `RenderContext.touch*` (the same struct the
seg masker writes into) and maintain the active/inactive flag. Velocity is
finite-differenced against the previous frame's point (as `seg_vx/vy` are).

The **built-in waveform** (the historical MilkDrop "touch adds a waveform"
behavior) is the *default action* gated by `touch_default`. Implementing that
waveform is a **prerequisite for the `touch_default` opt-in to mean anything** but
is NOT required for the read-only `touch_*` inputs to be useful. Scope it as a
separate sub-task:
- A small pool of touch waveforms (position, pressure→amplitude, lifetime).
- Rendered in addition to preset waveforms (per `touch.h` contract).
- `TouchDestroy` / release decays them.

### Multi-point note

Scalar `touch_*` = one touch. Pose arbitration (feature 2) deliberately collapses
to a single point, so scalar is sufficient for the motivating case. If multi-touch
is ever needed, either expose `touch_x0..N` or write points into the eval megabuf
as an array — deferred, not in this spec.

---

## Feature 2 — Pose-driven touch (host-side bridge)

Lives entirely host-side (sdl-test-ui, next to the seg / camera code). Consumes
pose keypoints, produces `projectm_touch` / `_drag` / `_destroy` calls. The engine
never knows pose exists — it only sees `touch_*` move.

```
  camera ─► pose model ─► [ per-hand scoring ]
                              │
                              ▼
                    [ per-hand activation gate ]   (hysteresis + dwell)
                              │
                              ▼
                    [ arbitration: sticky owner ]  (switch margin + dwell)
                              │
                              ▼
                    [ one-euro smoothing ]
                              │
                              ▼
             projectm_touch / _drag / _destroy  (single stream)
```

### Pose backend

Hardware decision (2026-07-09): **regular wide-angle webcam + monocular depth.**
The OAK-D Lite experiment was mixed and is dropped — no stereo/VPU depth. All
signals come from the single RGB stream:

- **Keypoints**: a 2D pose model on the RGB frame — MoveNet Lightning or BlazePose
  via ONNX (ANE on Mac, CUDA/TensorRT on the deployment box). Cost is cheap
  relative to the RVM matting already running.
- **Depth**: sampled from the **monocular depth map already computed for the seg
  depth gate** — sample it at each wrist keypoint. This is *relative/ordinal* depth
  (good enough for "which hand is closer / reaching forward"), not metric. No extra
  model run beyond what the seg path already pays. Feeds the closeness term and
  `touch_pressure`.

Required output per frame: for each hand, `(x, y, confidence)` in [0,1] screen
coords; `depth` sampled from the mono-depth map at the wrist; elbow/shoulder Y for
the "raised" term. Missing hand ⇒ confidence 0.

**Wide-angle caveats:** barrel distortion means raw keypoint coords may need
undistortion before use, and a performer can sit near the frame edge where both
pose confidence and mono-depth degrade — factor into `w_c` and the centrality of
the "raised" heuristic. Match whatever undistortion/mirror the seg path already
applies.

### Per-hand score

"How good does this hand look right now" — a weighted sum, analogous to the seg
gate's `salience = area × centrality`
([`segMask.cpp:1261`](src/sdl-test-ui/segMask.cpp#L1261)):

```
score_i = w_c · confidence          // keypoint conf; occluded/uncertain loses
        + w_m · motion              // wrist speed; a still hand isn't "performing"
        + w_d · closeness           // depth (OAK only); a deliberate reach wins
        + w_p · raised              // wrist above elbow/shoulder; gesture intent
```

- `motion` is the discriminating signal: it separates "waving to drive the visual"
  from "other hand hanging idle." Compute as smoothed wrist speed (frac/sec).
- `closeness` = mono-depth sampled at the wrist. It's relative/ordinal and noisier
  than stereo depth, so weight it modestly and smooth it; if the depth map is
  unavailable or low-confidence for a frame, fall back to `w_d = 0` and lean on
  `motion + raised`.
- All terms normalized to ~[0,1] before weighting. Start weights (tune live):
  `w_c=0.3, w_m=0.4, w_d=0.2, w_p=0.1`.

### Per-hand activation gate (independent per hand)

Asymmetric hysteresis + dwell so a hand can't flicker on/off. Same on/off logic as
the seg gate, per hand:

```
state: INACTIVE ──(score > S_on for T_on frames)──► ACTIVE
       ACTIVE   ──(score < S_off for T_off frames)─► INACTIVE       (S_off < S_on)
```

Suggested starting values (at ~30 fps): `S_on = 0.55`, `S_off = 0.35`,
`T_on = 4`, `T_off = 6`. When **no** hand is ACTIVE ⇒ `touch_on = 0` and emit
`TouchDestroy`.

### Arbitration: sticky owner (the new temporal axis)

The seg gate anchors spatially per frame; hands must stay anchored *across* frames
and switch only deliberately. Keep the current owner `W`; challenger `C` = highest
-scoring other ACTIVE hand.

```
switch W → C  when  score_C > score_W + MARGIN   sustained for T_switch frames
```

- `MARGIN` (~0.15) + `T_switch` (~5 frames) are what prevent ping-ponging when the
  hands are comparable — directly analogous to the seg gate's anti-hijack rule
  ([`segMask.cpp:1228-1234`](src/sdl-test-ui/segMask.cpp#L1228)) that stops a nearer
  fragment from stealing the anchor on a single frame.
- If `W` goes INACTIVE, immediately hand ownership to the best ACTIVE hand (no
  margin needed — the incumbent is gone).
- If no hand is ACTIVE, owner = none.

### Handoff artifact (owner W → owner C)

When arbitration switches hands, the waveform must do one of:

- **Jump (default)** — end A's waveform and spawn a new one at B
  (`TouchDestroy` + `projectm_touch`). Clean semantics; the waveform appears on the
  new hand rather than sliding across empty space. Simpler to reason about and the
  chosen default for now. Cost is a spawn/kill each switch — the arbitration margin
  + dwell keeps switches rare enough that this reads fine.
- **Drag** — `TouchDrag` straight to B's position; the waveform glides A→B.
  Smoother/more musical, but "teleports" through the gap between hands. Combined
  with one-euro smoothing the glide is damped. Available as an alternative.

Expose as a config toggle (`Touch Handoff = jump|drag`); default **jump**.

### Smoothing

Raw keypoints jitter. Run a **one-euro filter** on the owner's `(x, y)` before
emitting (min-cutoff ~1.0 Hz, beta ~0.01 — tune). This smooths the *emitted point*
per owner. Under `jump` handoff the smoother is reset on switch (new waveform, new
filter state); under `drag` it carries across so the glide eases rather than snaps.

### Emission state machine (drives the C API)

One machine for the whole bridge (owner is global):

```
NONE  ──owner appears──►  DOWN   (projectm_touch at owner x/y)
DOWN  ──owner moves────►  DRAG   (projectm_touch_drag each frame)
DRAG  ──owner switches─►  DOWN   (jump: destroy + touch at new owner)
                          DRAG   (drag: TouchDrag straight to new owner)
DRAG  ──no owner───────►  UP     (projectm_touch_destroy) ──► NONE
```

`touch_pressure` = owner closeness (OAK) or a constant (Mac). Update
`RenderContext.touch*` every frame regardless of state so `touch_vx/vy` stay
finite-differenced.

---

## Parameters summary (all live-tunable)

| Param              | Start | Role                                            |
|--------------------|-------|-------------------------------------------------|
| `w_c,w_m,w_d,w_p`  | .3/.4/.2/.1 | Score weights (conf/motion/depth/raised)  |
| `S_on / S_off`     | .55 / .35 | Activation gate thresholds                  |
| `T_on / T_off`     | 4 / 6 | Activation dwell (frames)                      |
| `MARGIN`           | .15   | Owner-switch score margin                       |
| `T_switch`         | 5     | Owner-switch dwell (frames)                     |
| one-euro min-cut/β | 1.0/.01 | Point smoothing                               |
| `Touch Handoff`    | jump  | jump \| drag                                    |

All frame counts assume ~30 fps; scale by fps if the pose rate differs.

---

## Phasing / resumable checkpoints

1. **Preset variables (library).** `RenderContext.touch*` fields + `PerFrameContext`
   register/reseed + `ProjectM::Touch` writes the point. No default waveform yet.
   Testable immediately by driving from the SDL mouse handler and reading `touch_x`
   in a preset. *~couple hours; useful on its own.*
2. **Built-in default waveform + `touch_default`.** Implement the touch waveform
   pool; gate on `touch_default`. Restores/defines the historical MilkDrop behavior
   as the opt-in default.
3. **Pose backend.** Add a 2D pose model (MoveNet/BlazePose via ONNX) on the
   wide-angle webcam stream; sample the existing seg mono-depth map at each wrist.
   Output `(x,y,conf,depth)` per hand into a host struct.
4. **Arbitration bridge.** Score → activation gate → sticky owner → one-euro →
   emission state machine → `projectm_touch*`. Reuse seg-gate hysteresis structure.
5. **Tune live.** Weights/thresholds/margins against real gestures; decide handoff
   default.

Phases 1–2 are library (likely upstreamable per the upstream strategy: keep
app/pose bits downstream). Phases 3–5 are sdl-test-ui / host only.

---

## Open questions

- **Two hands as two points?** This spec collapses to one arbitrated point. If a
  future preset wants both hands, revisit the multi-point surface (deferred).
- **Coordinate origin for pose vs. seg.** Confirm the pose model's Y origin and
  flip to bottom→top to match `seg_cy` / `touch_y` before feeding the bridge.
- **Mirror.** Camera is typically mirrored for the performer; decide whether the
  bridge or the capture layer applies the X flip (seg path already makes this
  choice — match it).
- **Default waveform fidelity.** How closely to replicate MilkDrop's exact touch
  waveform look, vs. a simpler engine-native waveform. Only matters for phase 2.
