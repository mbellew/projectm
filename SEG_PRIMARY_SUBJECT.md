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

## 4. Stage 1 — component verdicts (DONE, unverified in the field)

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
