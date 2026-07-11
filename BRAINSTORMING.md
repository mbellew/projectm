# Brainstorming — presets and ideas

Working notes. Not a plan, not a spec: a place to keep ideas, and the things we haven't built yet
that some of them depend on, so neither gets quietly forgotten.

## The premise: people want to see themselves

Recurring feedback from actual audiences: **"the ladies like to see themselves."** People want to
recognise themselves in the visuals. Most of the current presets are abstract fields that use the
person as a *modifier* — the seg matte perturbs a pattern — rather than as the *subject*.

### The rule that falls out of it

**Abstract the world, never the face.**

A preset can be as wild as it likes as long as the face survives. That is exactly why the flipbook
filters (esp. Harold) work and why `ghost` / `splitscan` / `blob-shadow` don't: those four eat the
person to feed the effect. Two supporting rules:

- **Mirror the image.** People must see themselves respond in the direction they expect. Unmirrored,
  every effect feels subtly wrong even when it's beautiful.
- **Make them big.** A small figure in a large field reads as abstract no matter the treatment.
  The flipbook's zoom-to-centroid is the right instinct.

### Where the current set actually stands

| Shows the person | Uses the person as a modifier |
|---|---|
| `recursive quad` (literal, matted, multiplied) | `ghost` (dissolves into the field) |
| flipbook filters (stylised, face survives) | `splitscan` (figure is an illegible smear) |
| `rorschach` (legible, though mirrored into a creature) | `blob-shadow` (silhouette only) |
| | `suksma` (edge-lit into abstraction) |

The abstract ones are *good* — several are the best-looking things in the set. The point is the
balance: we are heavy on "person as input", light on "person as subject".

## Audience notes

From people seeing it for the first time (these are worth more than our opinions):

- **"The ladies like to see themselves."** Recurring. See the premise above.
- **"Shapes interacting with the body — bouncing, exploding."** Nobody asked for prettier fields;
  they asked for *things that hit you*. This is a whole class we do not have: every current preset is
  a field that the body perturbs, and none contain an object with its own agency that collides with
  you, bounces off, piles up, sticks, or bursts. It is also the most *legible* kind of interaction —
  a ball bouncing off your shoulder needs no explanation, no affordance, and no learning.
- **"Relocating the body — stand or walk on a sphere."** Put the person somewhere else. Not a filter
  on the room: a *place*.
- **"Sparks between two hands."** The bold, obvious gesture — and notably the one people invent
  unprompted, which is exactly the affordance the gesture work has been missing.
- **"Extreme stretching — spaghettify as you fall into a black hole; warp the head."** They want the
  body *deformed*, not just decorated. Cartoon physics, not post-processing.

## Ideas

### Cheap and high-payoff (the pieces already exist)

- **Portrait mode.** Background blurred/bokeh'd, subject razor sharp, rim light, warm grade. The seg
  matte already gives the separation. The single most flattering thing a camera can do to a person,
  and everyone recognises the look from their phone.
- **Warhol grid.** 2x2 / 3x3 of the live matte, each cell a different flat duotone. Iconic, instantly
  readable; the tiling machinery already exists in `recursive quad`.
- **Sticker-you.** The matte with a fat white die-cut outline and a drop shadow, over a bold moving
  pattern. Cleanly separated, so the background can be as abstract as we like without eating the face.
- **Spotlight stage.** Dark room, hard spotlights tracking `seg_cx/cy`, swinging on the beat.

### Multiples (the `recursive quad` vein — this one demonstrably tests well)

- **Chorus line** — copies at beat-quantised delays. Largely built; the mirrored troupe is the good
  version.
- **Kaleidoscope, but legible** — wedges of *you*, with at least one upright unmirrored copy so the
  face is always available.

### Material treatments (body transformed, face intact)

- **Sequins / glitter fill** — body filled with a sparkling texture that catches light as you move.
- **Chrome / liquid metal** — `suksma` is halfway there; the difference is keeping the face readable.
- **Duotone risograph poster** — flat, printed; the flipbook filters proved this class works.

### Things that hit you (the strongest audience request)

- **Bouncing objects.** Balls / blocks / bubbles under gravity that collide with the silhouette and
  ricochet off. Immediately readable as cause and effect; needs no instruction.
- **Piling up.** Objects fall and settle *on* you — on your shoulders, head, outstretched arms — and
  slide off when you move. Invites people to pose and catch things.
- **Explosions on contact.** They burst where they touch you, on the beat.
- **Sticking / accreting.** They cling where they land, so you slowly become encrusted; shake to
  shed them.
- **Being repelled.** A swarm that avoids the body, so you carve a hole through it as you move.

### Relocate the body (put the person somewhere else)

The person stays photographic and legible; the *world* is replaced. This is a different axis from
stylising the person, and probably an underused one.

- **Standing / walking on a sphere.** A small planet under the feet, horizon curving away; the sphere
  rotates as you move, so walking spins the world. (Needs a ground contact point -- the bottom of the
  seg mask is a decent cheap proxy for "feet".)
- **Floating in space / underwater / a void** with the room stripped out entirely.
- **A drawn world.** Harold's world, but as the *setting* rather than the treatment: the person
  photographic, the world in marker.
- **Scale inversion.** Giant among tiny buildings, or tiny on a vast surface.

### Deform the body (cartoon physics, not post-processing)

They asked for the body itself to be *stretched and warped* — a different request from any effect we
have. Everything current is a filter or a field; nothing bends the person.

- **Spaghettification.** Falling into a black hole: the body stretches toward a singularity, radially
  smeared, faster the closer it gets. The seg matte gives the body; the deformation is a warp of the
  video sampling coords toward an attractor.
- **Random head warp.** Bulge / pinch / wobble the head. (Needs a head anchor — top of the seg mask
  is a cheap proxy; a real head joint would be better.)
- **Rubber / jelly.** The body overshoots and wobbles when you move — the same underdamped-spring
  trick used for the ghost lag, but applied to the *shape* rather than the position.
- **Melting, inflating, squash-and-stretch on the beat.**

### Short scenes and transition events

Most presets are loops: they run forever and end only when the clock says so. A few are **scenes** —
they have a beginning, a middle and an end, and they *finish*. `falling` is one; `flipbook` is another
(it holds, then zooms, then flurries through blank pages and hands off).

This is a pacing tool, and we are under-using it. A show made only of infinite loops has no rhythm:
nothing builds, nothing resolves, nothing surprises. Short scenes punctuate — they make the set feel
*authored* rather than shuffled.

The mechanism already exists: a preset can end itself by asserting `preset_complete` (a one-shot latch
the engine clears each frame — see the flipbook's lifetime code), and the engine can hand the last
frame to the next preset (`DrawInitialImage`).

Ideas for quick events, most of which are a few seconds long:

- **The floor drops away** — trapdoor; the person falls out of frame and the scene follows.
- **Sucked into a point** — everything (including the person) spirals into a vanishing point and is
  gone. Pairs naturally with the black-hole/spaghettify idea.
- **Shatter** — the image is glass; it cracks on a beat and falls out of frame.
- **Blackout with afterimage** — hard cut to black, the person's silhouette burning as a retinal
  ghost, fading.
- **Whip pan** — the camera snaps sideways and lands somewhere else entirely.
- **CRT power-off** — the picture collapses to a horizontal line, then a dot.
- **Film burn / projector jam** — the frame melts and the lamp shines through.
- **Explosion clears the screen** — on a big beat, the scene blows apart into the next one.
- **Zoom into the pupil** — push into an eye until the iris fills frame and becomes the next scene.

Two design notes that make them work:

- **Resolve to something neutral.** The flipbook ends on blank pages, so whatever comes next starts on
  a clean surface. An event that ends mid-chaos makes the next preset look like a glitch.
- **They should be RARE.** The value is punctuation. If every preset is an event, nothing is.

### Preset-authored transitions

Idea: a preset should be able to author *its own* entrance or exit. The flipbook's last page could
render the INCOMING preset and grow it to fill the frame. An outgoing preset could leave its trails
smearing while the new one slides in underneath.

This sounds like a lot of bookkeeping. It mostly isn't — **the engine already does the hard part**:

- Both presets really do render in parallel during a transition. The incoming preset gets its own
  `RenderFrame()` every frame alongside the outgoing one, into its own framebuffer
  (`ProjectM.cpp`, `m_transitioningPreset`).
- A transition shader already composes them, and already receives what such a shader would need:
  `iChannel0` = the OLD preset's output, `iChannel1` = the NEW preset's output, plus progress
  (`durationParams`), time, beat values and the noise textures (`PresetTransition::Draw`).
- There are already ~9 built-in transition shaders (circle, sweep, plasma, warp, blend...).

The only thing missing is **who chooses and writes that shader**. Today the engine picks one at
random (`m_transitionShaderManager->RandomTransition()`) and presets have no say.

So the feature is smaller than it looks: let a preset supply a transition shader as another code
block (a `transition_` block, exactly like `warp_` / `comp_` / `warp_pre_`), and decide who owns it:

- **Exit** (outgoing preset authors it) — the flipbook grows the new preset out of its last page; a
  scene collapses into the next one.
- **Entrance** (incoming preset authors it) — the new preset slides in, bleeds through, or hatches
  out of whatever was there.
- Rule needed when both have one: probably *entrance wins*, else the engine's random default.

#### What a transition shader gets today

The interface is Shadertoy-flavoured: a `mainImage(out vec4 fragColor, in vec2 fragCoord)`, with

- `iChannel0` = the OUTGOING preset's output, `iChannel1` = the INCOMING preset's output
- `iProgressLinear` / `iProgressCosine` / `iProgressBicubic` — the same progress, pre-shaped
- `iTransitionDuration`, `iTime`, `iTimeDelta`, `iFrame`, `iFrameRate`
- `iBass` / `iMid` / `iTreb` (+ `Att` variants) — transitions can be beat-driven
- `iRandStatic` (fixed for the transition) and `iRandFrame`; the noise textures

The six built-ins (Circle, Sweep, Warp, ZoomBlur, Plasma, SimpleBlend) are all geometric wipes over
two images. None is authored by, or aware of, either preset.

#### Transitions as loadable files

Make transitions a *directory* — `transitions/ZoomBlur.hlsl` etc. — loaded like presets, with the
current built-ins shipped as files (so they become editable examples rather than magic; keep the
embedded copies as a fallback so a missing directory can never leave the engine with no transition).
Presets then declare their own:

    transition_out = FlipbookPage     // my exit
    transition_in  = ZoomBlur         // my entrance

Mechanically this is small: `TransitionShaderManager` already compiles a fixed list and hands back a
random one; the change is to compile from files and let a preset name one.

The real content of the feature is the decisions, not the plumbing:

1. **Who wins?** Both presets can declare one. Rule: **`transition_in` wins** — the incoming preset's
   entrance takes precedence, then the outgoing preset's `transition_out`, then a random built-in.
   (The arriving preset knows how it wants to be arrived at; an exit is a suggestion, an entrance is
   a requirement. It also means a preset with a distinctive entrance behaves consistently no matter
   what preceded it.)
2. **Language.** Built-ins are GLSL/Shadertoy; presets are HLSL. If preset authors write transitions,
   they should be **HLSL with a `shader_body`** like every other preset block — don't make authors
   switch languages mid-file.
3. **Duration.** Currently global (`m_softCutDuration`). A page-turn exit wants ~0.5s, a slow bleed
   wants 4s: a preset-authored transition needs `transition_duration` too.
4. **Extra inputs — the one that matters.** The shader only sees the two output *textures*. To fold
   the incoming preset onto the flipbook's last page it needs the page geometry (the turn angle `q3`,
   the page rect). So the transition should also receive **the outgoing preset's `q1..q32`** (already
   in `PresetState`; just a uniform upload). Without it, a preset has to smuggle geometry through
   pixels.
5. **Inline vs named.** Named files give reuse; an inline `transition_out_1=` block gives bespoke
   one-offs. Both are cheap once the mechanism exists.

### Costume (needs joint positions — see *Not yet implemented*)

- **Wings / halo / crown** anchored to shoulders and head. Costume, not filter. People love costume.
- **Hand sparks** — trails and glitter from the wrists. The "I have powers" effect; the one most
  likely to make people actually *play*.
- **Arc between the hands.** Lightning / plasma strung between the two wrists, brightening as the
  hands come together or apart. Audience-requested, and the strongest gesture candidate we have: it
  is self-teaching (you see the arc, you move your hands, the arc responds), it needs no gesture
  *recognition* — just two joint positions — and it gives an immediate, physical cause-and-effect.

## Not yet implemented

None of these are walls -- they are just work we haven't done. Worth writing down so the ideas that
depend on them aren't quietly dropped as "impossible".

### Full pose is not passed to presets (yet)

A deliberate choice to keep the API surface small, not a limitation of the tracker. Presets see the
seg centroid and its velocity
(`seg_cx/cy`, `seg_vx/vy`, `seg_coverage`, `seg_valid`) and the mask channels
(`MaskSeg`, `MaskSegBlur`, `MaskMotion`, `MaskMotionDecay`). Full skeletal pose exists in the app
(the pose tracker) but reaches presets only indirectly, via the **pose→touch bridge** (`touch_x/y`,
`touch_vx/vy`, `touch_pressure`, `touch_on` — see `POSE_TOUCH.md`).

Costume ideas (wings, halo, hand sparks) need real joints. Options if we want them:
widen the API (a small fixed set — shoulders, wrists, head — not the whole skeleton), or keep the
surface small and pass a couple of *derived* scalars (arm span, hand height, hands-together).

### A motion VECTOR field is not exposed (yet)

`MaskMotion` is a magnitude only — "something changed here", with no direction. The mask texture is
already full (`r` seg, `g` segBlur, `b` motion, `a` motionDecay), so a flow field would need its own
channel or texture.

A per-pixel flow field (`MaskFlow(uv) -> float2`) would unlock a whole family of effects that are
currently impossible:

- advect a field *along* the direction you moved (paint smears the way you swipe)
- wind / fluid push — the pattern gets shoved by your motion, not just excited by it
- cloth and hair that trail correctly
- directional streaks and speed lines
- `blob-shadow`'s reaction field being *stirred* rather than merely seeded

Probably the highest-leverage thing on this list to build: it turns "the person is a source" into
"the person is a force".

### The mask cannot be sampled from per-frame / shape code (yet)

This is the one blocking the biggest audience request, so it is worth stating exactly.

Presets can already *simulate* objects: the eval language has persistent memory (`megabuf`,
`gmegabuf`), so particle state can be carried across frames, and custom shapes can be positioned from
per-frame code. What is missing is **collision**: per-frame and shape code can only see the body as a
single point (`seg_cx/cy`, plus `seg_coverage` as a size hint). The mask itself
(`MaskSeg`) is only reachable from the pixel shaders.

So today, an object can bounce off a *proxy* body (an ellipse or capsule around the centroid), but
not off the actual silhouette -- no bouncing off an outstretched arm, no landing on a shoulder.

What would unblock it, smallest first:

- **A point query from eval** -- `seg_at(x, y)` returning the mask value at a point. Enough for
  bouncing and settling.
- **Plus a gradient** -- `seg_normal_x/y(x, y)`, or just let the preset difference `seg_at` itself, to
  get a surface normal to bounce *off* (without it, objects can only stop, not ricochet correctly).
- **Alternative, no API change**: run the particles in the shader and keep their state in the feedback
  buffer. Possible, but painful, and hard to author.

### Gestures have not landed yet

Repeated attempts to make gestures *intentionally* drive a preset have struck out. Not obviously a
hard problem -- more likely we have been attacking it from the wrong end. Candidate reasons:

- **No feedback loop.** The person cannot see what the system thinks their hands are doing, so they
  can't learn the mapping. Nothing teaches them the gesture exists.
- **No affordance.** Nobody walks up to a visualiser and tries gestures unprovoked. The visual has to
  *invite* the movement (something following the hand invites you to move the hand).
- **Continuous scalars are hard to feel.** Mapping a gesture to a slider gives no moment of cause and
  effect. Impulses (a burst, a flash, a shockwave) are felt; slow parameter drift is not.
- **Recognition is brittle.** "Recognise a wave" is a research problem. Robust primitives are not:
  hand height, arm span, hands together/apart, hands above head, sudden lunge (speed). These are
  derivable from a couple of joints and essentially never misfire.

If we try again, the sequence that seems most likely to work: (1) put something visible on the hand
so the mapping is discoverable, (2) drive an *impulse*, not a parameter, (3) use a robust primitive,
not a recognised gesture.
