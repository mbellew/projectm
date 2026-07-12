# Pose API — exposing the skeleton to presets

Status: design (2026-07-10). Successor to the pose work in [`POSE_TOUCH.md`](POSE_TOUCH.md), which
delivered the *tracker* and a single arbitrated touch point. This spec covers the **preset-facing
surface**: giving preset code the joints themselves.

Motivating ideas (see [`BRAINSTORMING.md`](BRAINSTORMING.md)): costume (wings/halo anchored to
shoulders and head), **arc between the hands** (the strongest self-teaching gesture candidate — it
needs nothing but two joint positions), hand sparks, head warp, and body-relative deformation. All of
them are blocked on the same thing: presets cannot see a joint.

---

## Summary of decisions

1. **`pose(JOINT, VAR)`** — one eval host function, not ~100 named variables. Registered in **all
   five** eval contexts, so custom shapes and waves can read joints directly.
2. **Constants are `ALL_CAPS_WITH_UNDERSCORE`; live variables stay lowercase.** `pose(R_WRIST, X)`.
   This becomes a general convention (wave modes, blend modes, palette names, joint names).
3. **`seg_cx`/`seg_cy` revert to their original, unambiguous meaning** — the alpha-weighted matte
   centroid. The chest point becomes `pose(HEART, X)` instead of silently changing what "centroid"
   means.
4. **The pose→touch bridge stays as-is.** `touch_*` remains the simple, arbitrated single-point
   mapping — still the right answer for simple presets, and it is what the mouse drives too.
5. **Derived scalars** (`hands_apart`, `lunge`, …) ship alongside the raw joints, because per
   `BRAINSTORMING.md` those robust primitives are what actually make gestures land.

---

## 1. Why a function, not a slew of variables

The alternative — `pose_r_wrist_x`, `pose_r_wrist_y`, … — is ~20 joints × 6 vars ≈ **120 names**.

| | `pose(JOINT, VAR)` | ~120 variables |
|---|---|---|
| **Computed indices / loops** | ✅ `loop(JOINT_COUNT, … pose(j, X) …)` | ❌ impossible — every access is a literal name |
| API surface | 1 function + ~26 constants | ~120 registered names |
| Cost per eval context | one `register_function` | ~120 `register_variable`, in **every** context, and per-pixel contexts are instantiated **per worker thread** (`PerPixelContext::EnsureWorkerContexts`) |
| Adding a joint (e.g. fingers) | no API change | new names, new churn |
| Magic numbers | avoided by registered constants | n/a |

**Computed indices are the deciding factor.** projectm-eval is EEL2-flavoured and has `loop()` /
`while()`. Generic skeleton code — draw every bone, distribute costume along a limb, test a particle
against all joints — is exactly the shape of the blocked ideas, and it is only expressible with a
function.

### Precedent

This is not a new mechanism. `palette_r/g/b(knob, t)` already does exactly this
([`PaletteEvalFunctions.cpp`](src/libprojectM/MilkdropPreset/PaletteEvalFunctions.cpp)):

```cpp
projectm_eval_context_register_function(context, "palette_r", 2, &PaletteRed, ud);
```

and `RegisterPaletteFunctions` is invoked from **all five** eval contexts:

| Context | File |
|---|---|
| per-frame | `PerFrameContext.cpp:17` |
| per-pixel (per-vertex) | `PerPixelContext.cpp:18` |
| **custom shape** per-frame | `ShapePerFrameContext.cpp:18` |
| custom wave per-frame | `WaveformPerFrameContext.cpp:19` |
| custom wave per-point | `WaveformPerPointContext.cpp:19` |

That last point matters more than it looks: **custom shapes currently cannot see `touch_*`** (they
get only `q1..q32` and `t1..t8`), which is why `presets/tests/421-touch-paint.milk` has to bridge the
touch point through q-vars. Registering `pose()` the way `palette_*` is registered removes that
limitation for the skeleton — and costume *is* shape code.

---

## 2. The surface

### `pose(JOINT, VAR) -> float`

`argc = 2`. Returns 0 for an out-of-range JOINT/VAR (never traps).

### VAR

| Constant | Meaning |
|---|---|
| `POSE_X` | Position X, `[0,1]` left→right |
| `POSE_Y` | Position Y, `[0,1]` **bottom→top** (matches `seg_*` / `touch_*`) |
| `POSE_Z` | Closeness, `[0,1]` (1 = nearest), `-1` when no depth model is loaded |
| `POSE_CONF` | Confidence, `[0,1]`. **0 = not detected** |
| `POSE_VX` | Velocity X, screen-fractions/sec |
| `POSE_VY` | Velocity Y, screen-fractions/sec |

(Prefixed, not bare `X`/`Y`/`VX` — see the case-insensitivity warning in §3.)

`VX`/`VY` are not decoration: they are the **impulse** primitive. `BRAINSTORMING.md` concludes that
gestures land when they drive an impulse (a burst, a shockwave), not a parameter, and "sudden lunge
(speed)" is named as a robust primitive. Velocity is nearly free (finite-difference in the library,
exactly as `touch_vx/vy` and `seg_vx/vy` already do) and every preset would otherwise re-derive it by
hand in megabuf.

`Z` is also nearly free: the app already samples the monocular depth map at wrists
(`SegMasker::SampleDepth`); generalising to all joints is a loop.

### JOINT

Raw COCO-17, as the model emits them:

```
NOSE          0     L_SHOULDER  5     L_WRIST   9     L_KNEE   13
L_EYE         1     R_SHOULDER  6     R_WRIST  10     R_KNEE   14
R_EYE         2     L_ELBOW     7     L_HIP    11     L_ANKLE  15
L_EAR         3     R_ELBOW     8     R_HIP    12     R_ANKLE  16
R_EAR         4
```

Plus **derived** joints. These are the points presets actually want, and the raw skeleton does not
contain them:

| Constant | Definition | Why |
|---|---|---|
| `HEART` | shoulder-midpoint, dropped ~25% toward the hip-midpoint | The body's visual centre. Already computed today. |
| `L_HAND`, `R_HAND` | `wrist + (wrist − elbow) · reach` (≈0.45) | **COCO-17 has no finger keypoints — it stops at the wrist.** Painting at the wrist feels wrong; the extrapolated hand-tip is the point people expect. Already computed today. |
| `HEAD` | centre of eyes/ears, lifted above the nose | Anchor for halo / crown / head-warp. |
| `PELVIS` | hip-midpoint | Body root for deformation. |
| `JOINT_COUNT` | total | Loop bound. |

### Derived scalars (ordinary lowercase variables)

Registered like `seg_*`/`touch_*`. Small, fixed set — the "robust primitives" that per
`BRAINSTORMING.md` "essentially never misfire", as opposed to gesture *recognition*, which is
brittle:

| Variable | Meaning |
|---|---|
| `pose_valid` | 1.0 when a person is tracked, else 0.0 |
| `pose_hands_apart` | Distance between the hands, normalized |
| `pose_hands_together` | Smooth 1.0 as the hands close (the arc/spark gesture) |
| `pose_hands_height` | Mean hand height relative to the shoulders (>0 = raised) |
| `pose_arm_span` | Wrist-to-wrist distance |
| `pose_lunge` | Peak joint speed — the impulse trigger |

All `pose_`-prefixed, for the same reason the constants are: a bare `lunge` or `arm_span` shares a
namespace with every preset's own locals (§3).

The raw skeleton is for ambitious effects; these scalars are for the ones that work on the first try.

### Example

```
// Sparks arcing between the hands (the audience-requested gesture).
per_frame_1 = q1 = pose(JOINT_L_HAND, POSE_X);  q2 = pose(JOINT_L_HAND, POSE_Y);
per_frame_2 = q3 = pose(JOINT_R_HAND, POSE_X);  q4 = pose(JOINT_R_HAND, POSE_Y);
per_frame_3 = q5 = hands_together;              // brightness as they close
// ...custom shape interpolates instances along q1,q2 -> q3,q4.

// Costume: a halo at the head, only when confidently tracked.
// NOTE: shapes can call pose() directly -- unlike touch_*, which needs a q-var bridge.
shape_0_per_frame1 = x = pose(JOINT_HEAD, POSE_X);
shape_0_per_frame2 = y = pose(JOINT_HEAD, POSE_Y);
shape_0_per_frame3 = a = pose(JOINT_HEAD, POSE_CONF);   // fades out when the head is lost

// Generic: react to every joint (only possible with the function form).
per_frame_4 = j = 0; hot = 0;
per_frame_5 = loop(JOINT_COUNT,
                   spd = sqrt(sqr(pose(j, POSE_VX)) + sqr(pose(j, POSE_VY)));
                   hot = max(hot, spd * pose(j, POSE_CONF));
                   j = j + 1; );
```

### Semantics

- **Coordinates** are normalized `[0,1]`, Y bottom-up, **mirror already applied** — identical to
  `seg_*`/`touch_*`, so the three surfaces are interchangeable. The library owns the flip (as it does
  for the seg centroid); the app submits camera-native coords.
- **Missing joint** ⇒ `CONF` decays to 0 while `X`/`Y` **hold their last confident value**. (Snapping
  a lost joint to screen-centre would fling anything attached to it across the frame; holding lets a
  preset fade it out via `CONF` on its own terms.)
- **Smoothing** is library-side (EMA, matching `UpdateSegState`; optionally one-euro as the touch
  bridge does), with velocity finite-differenced from the smoothed position.
- **One person.** `pose()` describes *the performer* — the primary/arbitrated person, selected by the
  existing salience logic (box↔matte overlap + depth). Multi-person is deferred; if it ever lands it
  becomes `pose_n(PERSON, JOINT, VAR)` without disturbing this surface.

---

## 3. Constants — `ALL_CAPS_WITH_UNDERSCORE`, and they **must be prefixed**

A convention worth establishing repo-wide, not just for pose:

> **Lowercase = a live variable** that changes per frame (`bass`, `seg_cx`, `touch_x`, `hands_apart`).
> **ALL_CAPS = a compile-time constant** — an enum, a name, a mode (`JOINT_R_WRIST`, `POSE_X`).

Self-documenting: an author can tell at a glance whether an identifier is data or a name. It also
kills magic numbers — `pose(10, 0)` becomes `pose(JOINT_R_WRIST, POSE_X)`.

### ⚠ The eval language is CASE-INSENSITIVE — ALL_CAPS is *not* a namespace

`projectm-eval` compares identifiers with `strcasecmp`
([`TreeVariables.c:21`](vendor/projectm-eval/projectm-eval/TreeVariables.c#L21)). Therefore:

- A constant named `X` **is the same identifier as `x`** — the per-pixel mesh coordinate. Registering
  it would silently clobber the warp mesh.
- A constant named `VX` would collide with **preset locals**: presets freely invent short lowercase
  variables, and `Flexi + geiss - the deep diver's manifesto` literally contains `vx = vx*0.97 + …`.

ALL_CAPS therefore buys **readability, not isolation**. Every constant we register must be **prefixed**
so it cannot collide with a builtin or a preset's own locals:

- joints: `JOINT_NOSE` … `JOINT_R_WRIST`, `JOINT_HEART`, `JOINT_L_HAND`, `JOINT_HEAD`, `JOINT_PELVIS`,
  `JOINT_COUNT`
- vars: `POSE_X`, `POSE_Y`, `POSE_Z`, `POSE_CONF`, `POSE_VX`, `POSE_VY`

The same rule applies to any future enums (`WAVE_MODE_*`, `VIDEO_ALPHA_*`) — never a bare generic word.

Beyond pose, the same mechanism cleans up things that are integers-with-meaning today:

- wave modes (`nWaveMode = 7`) → `WAVE_MODE_*`
- palette selection by name
- blend/alpha modes (`video_alpha_mode = 0` → `VIDEO_ALPHA_SOURCE`)
- bone/joint names (this spec)

### Phase A — constants as registered variables (zero eval changes)

Register each constant with `projectm_eval_context_register_variable()` and write its value once at
context creation. It never updates.

- ✅ Works today, no `projectm-eval` change, no upstream risk.
- ⚠️ A preset *can* assign to it (`X = 5;`) and break itself. The `.milk` linter / VS Code extension
  should flag assignment to an ALL_CAPS identifier.
- ⚠️ Not folded — a variable read per access. Negligible in per-frame; measurable only in per-pixel
  inner loops.

### Phase B — real parser-side constants (a `projectm-eval` change)

The compiler **already has the machinery**: `prjm_eval_compiler_create_constant()`, and constant-expression
folding via `instr_is_const_expr` / `is_const_eval` / `args_are_const_evaluable`
(`vendor/projectm-eval/projectm-eval/CompilerFunctions.c`). What is missing is only a way to *name* one.

Add:

```c
void projectm_eval_context_register_constant(struct projectm_eval_context* ctx,
                                             const char* name, PRJM_EVAL_F value);
```

so the compiler resolves the identifier to a constant node instead of a variable slot. Then:

- constants **fold at compile time** (`pose(R_WRIST, X)` costs the same as `pose(10, 0)`),
- assignment becomes a **compile error** rather than a silent foot-gun,
- it generalises cleanly to wave modes, palettes, blend modes.

**Upstream note**: `projectm-eval` is vendored (`vendor/projectm-eval`) and is an upstream library.
Per the project's upstream strategy this is the kind of change that should go upstream rather than be
carried as a local patch — which is an argument for shipping **Phase A first** (it needs nothing from
the vendor) and treating Phase B as a follow-on.

---

## 4. Reverting the seg centroid

**Today** (shipped in `fc6a691d8`): the app overrides `seg_cx/seg_cy` with a pose-derived chest point
whenever a confident torso is present, and falls back to the alpha-weighted matte centroid otherwise.

That was a pragmatic fix for "the centroid sits at the belly-button", but it makes `seg_cx` mean **two
different things depending on whether a pose model happens to be loaded** — a preset can no longer
reason about what it is reading.

**Change**: revert it. `seg_cx`/`seg_cy` go back to being *exactly* the alpha-weighted centroid of the
seg matte, always, with no pose dependency. The chest is available — explicitly and unambiguously — as
`pose(HEART, X)` / `pose(HEART, Y)`.

Each name then has one meaning:

| Surface | Meaning |
|---|---|
| `seg_cx/cy` | Centroid of the matte. Works with **no pose model at all**. |
| `pose(HEART, …)` | The chest/heart joint. Requires pose. |
| `touch_x/y` | The single arbitrated interaction point (hand **or mouse**). |

Mechanically this is deleting the chest-override block in the `pmSDL` capture callback and restoring
the plain `projectm_video_set_seg_centroid(handle, cx, cy, coverage)` call.

---

## 5. Keeping the pose→touch bridge

`touch_*` is **not** superseded and should not be removed. It stays exactly as built and tuned:

- It is the **simple case**. A preset that just wants "a point that follows the person's hand" should
  not have to arbitrate hands, score confidence, or handle a missing skeleton. `touch_on` + `touch_x/y`
  is the whole API.
- It is **not pose-only**: the mouse drives the same path. A `touch_*` preset works on a laptop with
  no camera — a property `pose()` can never have.
- It carries **real logic** we do not want to re-litigate per preset: per-hand scoring, activation
  hysteresis, sticky-owner arbitration, one-euro smoothing (see `POSE_TOUCH.md`).

The two coexist: `touch_*` = one arbitrated point, ready-made; `pose()` = the raw skeleton, for
presets that want to do their own thing. `421-touch-paint.milk` is the reference `touch_*` consumer.

---

## 6. Wiring

Mirrors the `seg_*` path end-to-end; nothing here is a new mechanism.

```
PoseTracker (app, capture thread)
   → derive HEART / L_HAND / R_HAND / HEAD / PELVIS; sample Z from the depth map
   → projectm_pose_set(handle, joints, count)          [new C API, bulk, one call/frame]
        │  (atomic seq + plain stores, exactly like VideoSetSegCentroid)
        ▼
ProjectM::UpdatePoseState(dt)  (render thread)         [smoothing + velocity; cf. UpdateSegState]
        ▼
RenderContext / PresetState : PoseState { Joint[JOINT_COUNT]; derived scalars; }
        ▼
PoseEvalFunctions.cpp  (new; mirrors PaletteEvalFunctions.cpp)
   RegisterPoseFunctions(ctx, &poseState)  → "pose" host fn + ALL_CAPS constants
        ▼
   called from ALL FIVE contexts: PerFrame, PerPixel, ShapePerFrame,
                                  WaveformPerFrame, WaveformPerPoint
```

New/changed files:

| File | Change |
|---|---|
| `src/api/include/projectM-4/pose.h` | new: `projectm_pose_set()`, `projectm_pose_joint` struct, joint enum |
| `MilkdropPreset/PoseEvalFunctions.{hpp,cpp}` | new: `pose()` + constants; mirrors `PaletteEvalFunctions` |
| `Renderer/RenderContext.hpp` | new `PoseState` block (next to the `seg*` / `touch*` blocks) |
| `ProjectM.{hpp,cpp}` | `SetPose()`, `UpdatePoseState(dt)`, copy into `GetRenderContext()` |
| the 5 `*Context.cpp` | one `RegisterPoseFunctions(...)` call each + derived-scalar `REG_VAR`s |
| `sdl-test-ui/pmSDL.cpp` | fill + submit the skeleton; **revert the seg-centroid override** |

---

## 7. Follow-on: pose in the *shaders*

Out of scope here, but worth recording, because it is small and unblocks a named idea.

`touch_*` and (as specced) `pose()` live in the **eval** contexts only. The warp/comp **shaders** see
`seg_cx` etc. as uniforms (`MilkdropShader.cpp:308`) but have no touch or pose input. Consequently the
warp shader cannot advect *along the direction the hand moved* — `421-touch-paint.milk` advects along
the image gradient instead.

Exposing a few joints (and `touch_vx/vy`) as **shader uniforms** would deliver
`BRAINSTORMING.md`'s "paint smears the way you swipe" without building the full per-pixel `MaskFlow`
vector field. It is a handful of `SetUniformFloat` calls.

---

## 8. Phasing

1. **Library pose surface** — `PoseState` + smoothing/velocity + `pose()` host fn + `ALL_CAPS`
   constants (Phase A) + derived scalars, registered in all five contexts. *Upstreamable.*
2. **App** — fill the skeleton from `PersonPose`, derive the extra joints, submit via the C API;
   **revert the seg-centroid override**. *Downstream.*
3. **Constants Phase B** — parser-side constants in `projectm-eval`; generalise to wave modes,
   palettes, blend modes. *Upstream to projectm-eval.*
4. **Shader uniforms** — pose/touch in warp/comp for directional advection.

Phase 1+2 unblock the arc-between-hands, costume, and head-warp ideas. Phase 3 is ergonomics. Phase 4
unblocks directional paint.

---

## 9. Open questions

- **Missing-joint policy.** Proposed: hold last-known `X`/`Y`, decay `CONF` to 0. The alternative
  (ease to centre, as `seg_*` does) is wrong for anything anchored to a joint.
- **Smoothing strength.** `seg_*` eases hard; a wrist wants to stay responsive. Probably one-euro per
  joint (the bridge already has the filter) rather than the seg EMA.
- **`Z` without a depth model.** Return `-1` (as `SampleDepth` does) so a preset can branch, rather
  than a plausible-looking 0.
- **Person selection when the pose and the matte disagree** — reuse the bridge's box↔matte overlap.
- **Do custom waves really need `pose()`?** Registering costs nothing, but per-point code runs per
  sample; a preset looping the skeleton there could get expensive. Ship it, document the cost.
