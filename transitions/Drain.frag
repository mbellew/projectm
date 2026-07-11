// Drain — the OLD preset is sucked into a hole in the middle of the screen.
//
// The old frame stays on top and is DRAGGED into the centre, tearing into chunks as it goes, until
// nothing of it is left. The new preset is simply underneath the whole time: it is revealed from the
// EDGES INWARD, as the old image is stripped off them.
//
// Two things it is NOT, both of which were tried and are wrong:
//
//   - Opening a hole in the middle that shows the new preset. That reads as the new preset GROWING
//     out of the centre -- the opposite of something being swallowed by it.
//   - Scaling the old image down. A uniform scale is just a zoom-out: everything shrinks together
//     and nothing is being *pulled* anywhere.
//
// What it is: a LINEAR PULL. Each point is dragged toward the centre by the same DISTANCE, so we
// sample the old frame at (r + pull). The material nearest the hole goes down first, the rest
// follows it in, and the frame's outer edges are stripped away last. Where the source coordinate
// leaves the old frame there is no old image left to draw, so the new preset shows through.
//
// QUICK. Progress is cubed: it hangs almost still, then goes all at once.


const float WEDGES = 30.0;   // angular cells: how many spokes the tear breaks into

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 uv = fragCoord / iResolution.xy;
    float aspect = iResolution.x / iResolution.y;

    // Centred and aspect-corrected, so the swirl is round rather than an ellipse.
    vec2 p = uv - 0.5;
    p.x *= aspect;

    float r = length(p);
    float a = atan(p.y, p.x);

    // Hangs, then goes.
    float prog = iProgressLinear;
    float accel = prog * prog * prog;

    // THE SHARDS. Keyed by ANGLE ONLY -- one cell per wedge, running the wedge's whole length.
    //
    // This was the bug. Keying cells by (wedge, ring) gives every segment ALONG a spoke its own
    // random rate, so a spoke never moves as one shard -- it breaks into concentric bands sliding at
    // different speeds. That manufactures precisely the strong circular structure we don't want, and
    // destroys the radial shards we do. A shard must be ONE cell down its whole length so it is
    // dragged in as a single splinter.
    float wedge = floor((a / 6.2831853 + 0.5) * WEDGES);
    vec2 cellUv = vec2((wedge + 0.5) / WEDGES, 0.5) + float(iRandStatic.x) * 0.0013;

    // Two uncorrelated values per shard. One is not enough: if the same number sets both WHEN a shard
    // goes and HOW HARD it is pulled, they stay correlated and the tear comes out even.
    float j1 = texture(sampler_noise_lq, cellUv).x;                     // how hard this shard is pulled
    float j2 = texture(sampler_noise_lq, cellUv * 2.7 + 0.37).y;        // when it lets go, and sideways

    // The shard's leading edge is chewed rather than square-cut: high-frequency wobble along its
    // length, so the tip is jagged instead of a clean arc.
    float chew = texture(sampler_noise_lq, vec2(a * 1.9, r * 3.1) + float(iRandStatic.y) * 0.002).z;

    // A LINEAR PULL, not a scale. Every point of the old image is dragged toward the centre by the
    // same DISTANCE, so sample the old frame at (r + pull) rather than (r / k).
    //
    // The difference matters, and it is the whole effect:
    //   - a uniform scale is just a zoom-out: everything shrinks proportionally, nothing is "pulled".
    //   - a linear pull drags the material inward at a constant rate. The stuff nearest the hole
    //     goes down FIRST and vanishes through the centre, the rest follows it in, and the frame's
    //     outer edges are stripped away last -- revealing the new preset from the edges inward.
    //   - it also funnels for free: content from a ring of circumference 2*pi*(r+pull) is squeezed
    //     onto a ring of circumference 2*pi*r, so it COMPRESSES tangentially as it converges on the
    //     hole. That squeeze is what makes it look like a throat rather than a shrinking picture.
    //
    // (No swirl. A spiral was the obvious addition and it fought the idea -- once the image had
    // moved in, the twist was near-uniform across it and the whole thing read as a rigid card
    // spinning away. The tearing and the funnel squeeze do the work instead.)
    //
    // RAGGED needs three things, and doing only the first is what made the earlier version come out
    // smooth and symmetric:
    //
    //  1. cells pull at DIFFERENT RATES (j1) -- but on its own this just makes a slightly wobbly
    //     edge, because they all still start together and all move straight in;
    //  2. cells LET GO AT DIFFERENT TIMES (j2): a chunk sits there doing nothing while its
    //     neighbours have already gone down, then suddenly goes. This is what makes it tear rather
    //     than deform;
    //  3. cells are SHOVED SIDEWAYS as they go (j2 again, signed): purely radial motion is
    //     symmetric by construction, so chunks must slide off-axis or the whole thing stays neat.
    float letGo = 0.35 * j2;                                  // this chunk doesn't move until then
    float cellProg = max(accel - letGo, 0.0) / max(1.0 - letGo, 0.001);

    float pull = cellProg * (0.85 + 1.10 * j1);               // wide spread: some shards race ahead
    pull += (chew - 0.5) * 0.22 * cellProg;                   // ...and their tips tear unevenly
    float shove = (j2 - 0.5) * 0.40 * cellProg;               // tangential: breaks the symmetry

    vec2 ps = vec2(cos(a + shove), sin(a + shove)) * (r + pull);
    ps.x /= aspect;
    vec2 uvOld = ps + 0.5;

    vec3 imgNew = texture(iChannel1, uv).xyz;
    vec3 imgOld = texture(iChannel0, uvOld).xyz;

    // Where the source coordinate has left the old frame, there is no old image any more: the new
    // preset shows through. THIS is what makes the old collapse inward rather than the new grow out.
    vec2 edge = min(uvOld, 1.0 - uvOld);
    float haveOld = smoothstep(0.0, 0.004, min(edge.x, edge.y));

    // FOLDING. Without shading this is a flat sheet sliding around; the darkening is what sells it
    // as material bending down a funnel.
    //   creases along the wedge boundaries -- each wedge is a panel, dark at its folds
    float wedgeT = fract((a / 6.2831853 + 0.5) * WEDGES);
    float toFold = abs(wedgeT - 0.5) * 2.0;
    float crease = 1.0 - 0.45 * toFold * accel;
    //   ...and it darkens as it goes down. Kept mild ON PURPOSE: a strong radial gradient is itself
    //   a circular cue, and this transition already had too much roundness. The creases (which run
    //   along the shards) carry the folding instead.
    float shade = 1.0 - 0.40 * accel * (1.0 - smoothstep(0.0, 0.6, r));

    imgOld *= crease * shade;

    // Kill any last shreds so the transition always finishes clean.
    float oldAlpha = haveOld * (1.0 - smoothstep(0.9, 1.0, prog));

    fragColor = vec4(mix(imgNew, imgOld, oldAlpha), 1.0);
}
