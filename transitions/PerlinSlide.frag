// PerlinSlide — dissolve along a SLIDING slice of 3D Perlin noise.
//
// Two things happen at once:
//
//  1. A rising threshold sweeps through the noise field. Where the noise still stands ABOVE the
//     threshold you see the OLD preset; where the threshold has passed it, the NEW one. So the new
//     image doesn't fade in, it *floods* in — valleys fill first, ridges hold out longest, and the
//     boundary is an organic wandering coastline rather than a wipe.
//
//  2. The noise field SLIDES. It drifts across the frame (and through the volume's third axis) as
//     the transition runs, so the coastline travels and churns instead of merely filling in place.
//     Without this the dissolve is static and mushy; the drift is what gives it direction and life.
//
// The blend band is deliberately NARROW: a wide one is just a cross-fade with lumpy timing. The
// point is a visible, travelling edge.


void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 uv = fragCoord / iResolution.xy;

    vec3 imgOld = texture(iChannel0, uv).xyz;
    vec3 imgNew = texture(iChannel1, uv).xyz;

    // Per-transition randomness (iRandStatic is fixed for the whole transition, so the field does
    // not jitter frame to frame): feature size, which slice we cut, and how hard the edge is.
    float scale = 1.5 + mod(float(iRandStatic.x) * 0.01, 3.5);     // ~1.5 .. 5 blobs across the frame
    float slice = mod(float(iRandStatic.y) * 0.001, 1.0);          // where in the volume we start
    float blend = 0.05 + mod(float(iRandStatic.z) * 0.0001, 0.06); // narrow: a travelling edge, not a fade

    // ...and a random slide direction, so the field drifts a different way each time.
    float ang = mod(float(iRandStatic.w), 6.2831853);
    vec2 dir = vec2(cos(ang), sin(ang));

    // Keep the aspect square-ish so the blobs aren't stretched on a wide frame.
    vec2 nuv = uv * scale;
    nuv.x *= iResolution.x / iResolution.y;

    // THE SLIDE. Drift the field laterally as the transition runs, and creep through the volume's
    // z axis at the same time -- the lateral drift moves the coastline, the z creep makes it churn
    // and reshape as it goes, so it never looks like a rigid stencil being dragged across.
    nuv += dir * iProgressLinear * 0.9;
    float z = slice + iProgressLinear * 0.25;

    float n = texture(sampler_noisevol_hq, vec3(nuv, z)).x;   // 0 .. 1

    // Sweep the threshold past BOTH ends of the noise range, padded by the blend width, so the
    // transition genuinely starts fully old and finishes fully new -- if the threshold only ran
    // 0..1, the extreme peaks and valleys would never flip and you'd be left with ghost patches.
    float edge = mix(-blend, 1.0 + blend, iProgressLinear);

    // 0 where the noise still stands above the threshold (old), 1 where it has been submerged (new).
    float reveal = 1.0 - smoothstep(edge - blend, edge + blend, n);

    fragColor = vec4(mix(imgOld, imgNew, reveal), 1.0);
}
