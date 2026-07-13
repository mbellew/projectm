# Primary-Subject Selection in the Foreground Mask

How the depth gate decides *who is the show* — and how pose will sharpen that decision without
becoming load-bearing for it.

Companion to [VIDEO_MASKING_PIPELINE.md](VIDEO_MASKING_PIPELINE.md) (the mask back-end),
[SEG_MASK_PERF.md](SEG_MASK_PERF.md) (what it costs) and [POSE_API.md](POSE_API.md) (the skeleton).

---

## 1. The problem

The class-aware matte (RVM / U²-Net) happily keeps *every* person it finds — spectators,
passers-by, someone crossing the back of the room. The monocular depth gate
(`SegMasker::ApplyDepthGate`, `src/sdl-test-ui/segMask.cpp`) exists to drop the ones who are
behind the subject. Two failure modes were observed in the field:

**(a) A background figure comes out sliced.** Half a body survives, half is cut. It reads as a
rendering bug, not as a masking decision.

**(b) An incomplete side-figure pulls focus.** Someone half-out of shot at the left or right
edge, close to the camera, becomes the anchor and pushes the real central subject behind the
keep band.

## 2. Root cause: the gate has no concept of a person

Both failures are the same defect. The gate reasons about **cells** and **blobs**; nothing in
the pipeline models *a person as an object*.

- **(a)** was a per-cell depth threshold. The code computed each connected component's median
  closeness and then *discarded it*, thresholding every grid cell against its own depth
  instead. A body lying across the band edge — a slight depth gradient, a torso the depth model
  is noisy across — had some cells above the cutoff and some below. So half of them survived.
  Nothing in the algorithm said "these cells are one body."
- **(b)** was `salience = area x centrality`. `area` is raw cell count, which grows with the
  *square* of nearness, so a body looming at the lens outranked a whole centred figure on bulk
  alone — the exact pathology the centrality term was added to fight. And nothing penalized a
  figure that was obviously truncated.

## 3. The inversion (the load-bearing design decision)

The obvious fix is to make **pose** the foundation: person instances, per-person verdicts, the
skeleton as the anchor. **This is wrong, and it is wrong for a non-obvious reason.**

> The person most likely to be sliced is the person least likely to have a skeleton.

Failure (a) is about *background* figures. Background figures are exactly who YOLO-pose misses:
smaller, partially occluded, motion-blurred, and — this being a music visualizer — backlit,
strobed, and in the dark. If pose is the foundation and the depth band is the fallback, then
every case we are trying to fix falls through to the fallback, and the fallback is the broken
code. Pose recall is *anti-correlated* with the failure it would be fixing.

So:

| Layer | Role |
|---|---|
| **Component-level depth gate** | **The foundation.** Degrades gracefully; covers *everyone*, including whoever pose can't see. |
| **Pose** | **A refinement.** Adds identity, hysteresis, and merged-blob splitting *where recall permits*. |

That inversion is what makes the dance-floor and missed-detection cases fail *soft* instead of
failing *weird*. It also means Stage 1 below is not scaffolding to be thrown away — it is the
permanent fallback path that the instance layer needs underneath it anyway.

## 4. Stage 1 — component verdicts (DONE, **now verified** — see §10)

No pose. Rewrites the decision half of `ApplyDepthGate`; the depth model, the coarse grid, the
connected-component pass, the keep-mask grow and the GPU hand-off are unchanged.

**Per-component verdicts.** Every cell of component *k* is weighed by that component's **median**
closeness, not its own. A person is one object and gets one answer. Cells in no component fall
back to the per-cell rule — they only exist under the soft matte fringe, where the `grow`
dilation and the AND with the full-res alpha decide the edge regardless.

*Known limit, accepted:* two people **merged** into one component by a touch or an overlap share
a verdict. In practice people who merge are at similar depth (that is *why* they merged), so the
shared verdict is usually right for both. Splitting them needs an instance signal → Stage 3.

**Salience, reworked:**

```
salience = sqrt(area) x centrality x (sideClipped ? 0.4 : 1)
```

- `sqrt(area)` — makes size *evidence*, not a trump card (see §2).
- **Side-clip only.** A component is penalized when it runs off the **left or right** border —
  never the top or bottom. People are vertical and the frame is horizontal, so the main subject
  routinely runs off the top (head, hair) and the bottom (feet); penalizing that would punish
  precisely the figure we want to anchor on. The impostor this guards against enters from the
  side. *(This distinction is the whole value of the term — a naive "touches any border" test is
  worse than no test at all.)*

**Anchor stickiness.** The incumbent anchor's score gets up to a ×1.4 bonus, decaying with
distance from where it sat last frame. A challenger must *clearly* win rather than merely tie and
swap on depth noise. A subject who walks keeps the anchor; one who is genuinely replaced loses it.

**Reference EMA.** `refClose` is smoothed at 0.15/frame (~0.3 s at 30fps). Even a legitimate
hand-off re-focuses instead of cutting, and residual per-frame noise in the anchor's median stops
propagating into every other component's verdict.

**P05/P95 normalization** (replacing raw min/max). Not on the original list, and quietly the most
valuable item here. Closeness was normalized by the frame's single nearest and single farthest
*pixel*: a hand reaching toward the lens, or a door opening onto a deep hallway, rescaled the
entire frame's closeness and with it every downstream threshold — so `refClose - band` wobbled
even when nobody moved. The *coordinate system itself* was jittering, and no amount of smoothing
layered on top of a jittering coordinate system helps. Percentiles fix it at the source; out-of-
range pixels clamp rather than distorting a median.

### Tuning

`PROJECTM_SEG_DEPTH_DEBUG=1` logs, every ~60 frames, each anchor-eligible component: median
closeness, salience, `|` if side-clipped, `*` if it is the anchor, and the keep weight it
received. **A component whose keep is neither ~0 nor ~1 is sitting on the band edge and is the
one that will flicker.**

| Env | Default | Meaning |
|---|---|---|
| `PROJECTM_SEG_DEPTH_BAND` | 0.20 | how far behind the anchor still counts as "front" |
| `PROJECTM_SEG_DEPTH_SIDECLIP` | 0.4 | salience multiplier for a side-clipped component; 1 disables |
| `PROJECTM_SEG_DEPTH_CENTER` | 1.0 | centrality falloff; 0 = pure largest-blob anchoring |
| `PROJECTM_SEG_DEPTH_MINAREA` | gn/400 | below this a component never anchors (it still gets a verdict) |
| `PROJECTM_SEG_DEPTH_GROW` | 1 | keep-mask dilation, in grid cells |

**Status: builds, runs, no regression in the single-person case. The multi-person fix is
UNVERIFIED** — it needs a scene with a background figure and a side impostor. Verify before
building Stage 2; the honest expectation is that Stage 1 resolves most of what was observed.

### macOS caveat

CoreML **rejects** the Depth Anything ViT outright (`unbounded dimension which is not
supported`), so the depth model falls back to CPU on macOS. Judge the gate's *cost* on the CUDA
box, not here.

## 5. Stage 2 — pose as refinement

Cheaper than it looks, because most of the machinery already exists.

**Already there:** pose runs every frame, on the same capture thread, on the *same RGB buffer*
the masker built (`pmSDL.cpp`, `masker->RgbFrame()`), so reordering it before the gate costs
nothing. Each joint's `z` is *already* `masker->SampleDepth(jx, jy)`. The library already EMAs
the skeleton (`ProjectM::UpdatePoseState` — position, velocity, confidence, 0.5 s stale timeout,
holds a joint's position when it drops out). And `PoseTouchBridge::Track` already implements
greedy nearest-neighbour association with a miss tolerance and sticky owner arbitration — it just
tracks **wrists** instead of **persons**, and its IDs never flow back.

**Missing: identity above the joint layer.** `PoseTracker` is stateless per frame and the
"primary" person is literally `poses.front()` — the top NMS score, re-picked from scratch every
frame. Two similarly-scored people flip the entire skeleton frame to frame. *This is failure (b)
appearing a second time, in the pose API, independently of the gate.*

Note the system currently holds **three independent notions of "the subject"** that can disagree:
the gate's anchor component, `poses.front()`, and the matte centroid behind `seg_cx/seg_cy`. The
real deliverable of Stage 2 is not better masking — it is **one `PrimarySubject` decision, with
hysteresis, consumed by all three.**

Person score (lift the tracker from wrists to persons first — it is a hard prerequisite, not
garnish):

```
S = detScore · C_torso² · sqrt(bboxArea) · centrality(torsoMid) · (1 − 0.7·sideClip)
```

`C_torso` = mean confidence of {shoulders, hips, nose}, **squared** so a half-cropped side figure
falls off a cliff. Centrality on the shoulder–hip midpoint, not the pixel blob. Hysteresis: EMA
each score (τ ≈ 0.5 s); the incumbent holds until a challenger exceeds ~1.4× for ~20 consecutive
frames. If the incumbent's track dies abruptly, **freeze `refClose` for ~1 s before re-electing** —
a pose blink must not re-anchor the scene.

**Two freebies that come with it:**
- **Auto depth-invert.** The subject's keypoints should be *closer* than the scene median. If
  they are reliably not, the depth sign is flipped — deletes `PROJECTM_SEG_DEPTH_INVERT` as a
  configuration failure mode.
- **Matte health.** The fraction of confident keypoints landing on alpha ≥ 0.5. When it collapses,
  the seg model is failing (lighting shift, stale RVM recurrent state) — freeze the gate rather
  than gate on garbage. Also the natural "nobody's home" signal: no poses + near-empty matte.
  Today the gate will cheerfully anchor on a jacket draped over a chair.

## 6. Stage 3 — merged-blob splitting (only if the field still shows slicing)

For two people fused into one component at *different* depths. **Seeded geodesic labeling**, not
energy minimization: a multi-label graph cut is affordable at 30fps but it is the wrong tool —
the unaries (monocular depth, sparse keypoints) are an order of magnitude noisier than any
pairwise smoothness term could clean up, and the label churn at seams is exactly what a
viewer-facing mask must not do.

- **Seeds:** rasterize each tracked person's **limb segments** into the grid — bones with 2–3
  cells of thickness, torso as the filled shoulder–hip quad. **Never bboxes**, which overlap
  wildly in crowds and would claim other people's matte.
- One multi-source BFS/Dijkstra **restricted to foreground cells**, all seeds at distance 0.
- Edge cost between adjacent foreground cells: `1 + k·|Δcloseness|`, so the seam between two
  touching people snaps to the depth discontinuity rather than the Euclidean midline.
- Cells unreachable through foreground → unclaimed → Stage 1's per-component verdict.

Microseconds on ~80k cells, deterministic, and it respects matte topology — which incidentally
fixes a case nobody listed: **a subject holding a guitar.** The prop is far from any skeleton, so
Euclidean nearest-skeleton would orphan it; a foreground-restricted distance transform claims it
for the subject, because it is connected *through the matte*.

## 7. Temporal rules (applies to any per-person verdict)

**"All-in or all-out per person" means spatially uniform, temporally a fade.** A whole body
popping in and out is far more visually violent than the slice we set out to fix. When someone
walks behind the subject, their matte splits, pose flickers, and a hard binary verdict strobes
them.

- **Schmitt trigger** on the keep decision (enter/exit thresholds separated by ~0.05 of the
  robust spread); between them, hold the previous verdict.
- **Dwell:** a flip must persist ~8–10 frames before the fade even starts.
- **Asymmetric fades:** in fast (~200 ms — a missing subject is the worse error), out slow
  (~500–800 ms — reads as intentional).
- **Quantize fade *starts* to beat boundaries** once [BeatDetect](MUSIC_GATE.md) lands and
  confidence is high. A person dissolving on the downbeat reads as a VJ decision; the same
  dissolve mid-phrase reads as a glitch. This is the cheapest "feel intentional" trick available,
  and nothing else in this problem space has a beat clock to hand.

## 8. Crowd mode (explicitly out of scope for per-person culling)

On a dance floor — one giant merged component, pose finding 5 of 9 people — nearest-skeleton
assignment hands a missed person's cells to whichever detected neighbour is closest, and no
labeling formulation fixes *missing seeds*. But the honest answer is that in crowd conditions
**per-person culling is the wrong product behaviour anyway: everyone is the show.** Detect the
crowd (≥N tracked people, or matte coverage over a threshold) and **widen the band or stand the
gate down** — do not pretend the instance layer works there.

## 9. Explicitly rejected

| Idea | Why not |
|---|---|
| Pose as the *foundation* of the gate | §3. Recall is anti-correlated with the failure. |
| Energy minimization / graph cut / CRF | Unaries are far noisier than the smoothness term can fix; seam churn. |
| Bounding boxes as person support regions | Overlap wildly in crowds; claim other people's matte. Use limb rasterization. |
| Trimap seeding from pose | RVM does not take a trimap. |
| Penalizing top/bottom frame contact | The main subject clips top and bottom *by construction*. §4. |
| Per-person gating in a crowd | §8 — stand the gate down instead. |


---

# 10. Validation and what actually shipped (2026-07-13)

Stage 1 was landed "UNVERIFIED against a real multi-person scene". It has now been verified, four
further defects were found and fixed, and Stage 2's association half is done. Everything below is
reproducible.

## The rig (this is the load-bearing part)

`PROJECTM_VIDEO_FILE=<image | directory>` (`videoCapture_linux.cpp`) replays frames from disk through
the *same callback* the camera uses, so seg / depth gate / pose / the anchor's temporal state all
behave exactly as they do live. **A live camera cannot validate a subject gate**: judging it needs the
same background figure in the same place across two builds. With the rig, the pre-change gate and the
post-change gate can be run against identical input and the difference attributed.

Scenes were composited from a real capture of the room (subject cut out with its own matte, rescaled,
re-pasted with correct ground contact so monocular depth reads them sensibly):

| scene | what it stages |
|---|---|
| `A_control` | the subject alone |
| `B_background_figure` | a smaller figure further back |
| `C_side_impostor` | a large figure clipped by the LEFT border, nearer than the subject |
| `D_both` | B + C together |
| `E_touching_figure` | a background figure visually TOUCHING the subject |
| `seq_nearfar/` | 2-frame loop: subject near, subject far (forces the refClose EMA to lag) |

`PROJECTM_SEG_MARKERS=1` stamps the two independent elections into the frame: **magenta** = the depth
gate's anchor, **cyan** = pose's primary. If they land on different people, the mask is keeping one
person while the paint follows another.

## Stage 1 verified: both original failures were real, and one was catastrophic

- **Side impostor (C).** The OLD gate anchored on the impostor (salience 12145 vs the subject's 10843
  — raw area is a trump card), set the band from the impostor's depth, and **erased the subject
  entirely** — only faint feet survived. The new salience (`sqrt(area) x centrality x sideClip`)
  anchors on the subject (102.9 vs 36.9) and it comes through whole.
- **Sliced background figure (B).** The OLD gate rendered it with a gradient down the body (head
  faded, legs bright) — the per-cell thresholding artifact. Per-component verdicts render one uniform
  answer. Confirmed.

## Four defects found and fixed on top

1. **A dropped body did not go to zero** (`94fa04d1`). It came out at `keep=0.22` — a visible grey
   ghost — because the component's median was still fed through the soft *spatial* ramp. Verdicts are
   now decisive, with a Schmitt trigger (a body ON the edge would otherwise flip every frame) and a
   uniform per-component fade. **The fade must live on the COMPONENT, not the cell grid**: an EMA over
   the grid would ghost every fast-moving arm as it swept into cells that were previously background.
   Two further bugs fell out of the same block: the silhouette **fringe voted to save itself** (fringe
   cells join no component, fall back to the per-cell rule, and monocular depth bleeds *outward* across
   a silhouette so they read "near" — a dropped figure kept a thin outline of itself), and **`grow`
   resurrected the outline** (a 4-neighbour MAX pushed a 1 straight back into the body just dropped).
   Also: components below `minArea` were being **deleted outright** (`keepWeight(-1)` scores as
   infinitely far), silently erasing e.g. a hand cut off from the body by an occlusion.
2. **The anchor could be erased by its own band** (`a68bcc88`). The band's origin is the *smoothed*
   reference but components are judged on their *measured* median, and the EMA lags. Step back from the
   camera and closeness drops faster than the EMA follows, so the edge rises above the anchor's own
   closeness and **the gate deletes the person it anchored on**. Observed live:
   `anchor=3 refClose=0.42 (raw 0.18), keep>=0.22; [3:* close=0.18 keep=0.00]`. Fixed by clamping the
   origin to `min(EMA, measured)`: the EMA can now only lag *nearer*, which is permissive, never
   erosive. **Invariant: the anchor defines the band, so the anchor can never fall outside it.**
3. **Touching blobs merged into the subject** (`6fd52e6e`) — the user-reported failure, reproduced as
   scene E (`1 comps`, n=14767 = subject 10900 + figure 3300, fused, one verdict, figure survives).
   Fixed with a **depth seam in the CONNECTIVITY**, not a stricter threshold: the flood fill refuses to
   cross a depth discontinuity. A body's depth varies smoothly so nothing inside it crosses the seam;
   an object merely *touching* it has a step at the contact and is cut loose to be judged on its own
   median. `PROJECTM_SEG_DEPTH_SEAM`, default 0.20, swept over 0.06/0.10/0.15/0.20. **This is NOT the
   per-cell rule** — the verdict stays per component; only connectivity became depth-aware.
   *Known residual:* a blob that matches the subject in colour AND sits at their depth (skin on skin)
   is beyond the gate's reach by construction — RVM merges it into the matte and depth has no
   discontinuity to find. That needs an instance signal, not a better depth rule.
4. **The band went negative when the subject was far** (`f056b933`). `band` is an ABSOLUTE slice of a
   RELATIVE (P05/P95-normalized) coordinate system, so a subject reading 0.19 has less than `band`
   (0.20) of range behind them: the edge goes negative, everything passes, and **the gate silently
   stops gating exactly when a performer steps back**. Observed live: `refClose=0.19 ... keep>=-0.01`.
   Fixed with a proportional floor, which has a physical basis: Depth Anything emits INVERSE depth, so
   closeness ~ 1/distance and an additive band means a wildly different real-world distance depending
   on where the subject stands. `keepEdge = max(origin - band, origin * bandFrac)` keeps the additive
   rule wherever it is well-behaved (every validated scene is bit-identical) and reads as "drop anyone
   more than 1/bandFrac times farther than the subject" (default 0.5 => twice as far).

## Stage 2, association half: DONE (`afd3c9a3`)

`poses.front()` was the top NMS score, re-elected every frame. It is now **the skeleton that lands on
the body the gate kept** — each pose scored by its confidence-weighted keypoint overlap with the
anchor component (`SegMasker::InAnchor`), best fit promoted to the front, so every downstream consumer
follows. Association only; the gate's election is untouched, so a subject with no skeleton (turned
away, crouched, occluded) is never penalized — §3 still holds.

**This was not theoretical.** On the live camera, in ~10% of frames YOLO detected two people and NMS
ranked *first* a phantom with **zero** overlap with the subject (`fit 0.00` vs the subject's 0.90-1.00)
— most likely the figure in the framed artwork on the wall. The mask kept the performer while the touch
bridge, and therefore the painting, followed a picture. On scene C the phantom was the side impostor:
`[Pose] primary <- #1 of 2 (fit 1.00 to the gate's anchor; NMS would have picked #0, fit 0.00)`.

## Two bugs the STATIC rig could not have caught

Both were caught only by running the live camera, and both are worth remembering when trusting a rig:

- **Concave bodies.** The per-component fade first sampled last frame's weight at the component's
  *centroid* — but a body is routinely concave (arms out, legs apart, a torso around a desk), so the
  centroid lands in a hole that is not part of the component. The fade then read "was dropped" every
  frame and **the performer sat at keep=0.30**. The composited scenes all had a convex standing figure
  and would have shipped it. The incumbent weight is now the MEAN over the component's own cells.
- **The anchor erasure (defect 2)** needed the subject's depth to change faster than the EMA follows —
  i.e. motion. It was reproduced afterwards with the `seq_nearfar` loop, but it was *found* live.

## Cost

Depth stage 7.2 -> 7.8 ms (fringe pass + seam). Frame total ~28 ms at 640x480 on the RTX 5060, against
a 33 ms camera interval. See SEG_MASK_PERF.md.

## Still open

- **Stage 2's feedback half (B):** letting pose *quality* feed back into anchor SELECTION. Deliberately
  not done. It is a positive feedback loop and it re-opens §3 through the side door: the performer who
  turns away loses their skeleton exactly when a well-lit spectator keeps theirs, so a naive bonus
  re-anchors on the spectator. If attempted: pose may only ever ADD (`1 + w*fit`, bounded), never
  subtract — absence of a skeleton must not be evidence of absence — and each candidate component must
  be scored by its own best-fitting pose, not the incumbent's, or it locks in.
- **The three notions of the subject** are now two: the gate's anchor and pose agree. `seg_cx/seg_cy`
  (the matte centroid) is still elected independently and can still disagree with both.
