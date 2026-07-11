// HorizontalHold — the CRT loses horizontal hold, tears itself apart, and re-locks on a new channel.
//
// The failure is specific, and getting it right is the whole point: a TV losing horizontal hold does
// not fade or wipe. Each scan LINE starts at the wrong place, so the picture shears into horizontal
// bands that slide sideways and WRAP around the screen — the image keeps rolling off one edge and
// re-entering the other, tearing worse and worse, until the set finds sync again.
//
// Structure:
//   - the OLD signal progressively loses hold: shear and slip ramp up, and it starts to roll.
//   - at peak chaos the tube is fed the NEW signal, which arrives just as broken...
//   - ...and then LOCKS: its shear collapses to zero and the picture snaps rigid.
//
// The lock is deliberately a snap, not a fade. A CRT does not ease into sync; it hunts, catches, and
// holds. The last 20% of the transition is that catch.

const float LINES = 240.0;   // scan lines: coarse enough that bands are visible, not a blur

// One line's horizontal displacement. Three failures stacked, because a real one is never tidy:
//   fine   -- every line off by a little (the "torn edge" look)
//   bands  -- clumps of lines slipping together (the picture breaking into slabs)
//   roll   -- the whole image sliding sideways and wrapping (hold lost entirely)
float lineOffset(float row, float t, float chaos, float seed)
{
    float fine = texture(sampler_noise_lq, vec2(row / LINES, t * 0.7 + seed)).x - 0.5;

    // Bands: a low-frequency noise, thresholded, so slips affect a RUN of lines rather than each
    // line independently. This is what makes it read as a picture breaking apart instead of static.
    float bandNoise = texture(sampler_noise_lq, vec2(row / LINES * 0.18, t * 0.35 + seed)).y;
    float band = step(0.62, bandNoise) * (bandNoise - 0.5);

    float roll = t * 0.35;   // hold lost: the picture just keeps sliding

    return (fine * 0.30 + band * 1.60) * chaos + roll * chaos * chaos;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 uv = fragCoord / iResolution.xy;
    float prog = iProgressLinear;
    float seed = float(iRandStatic.x) * 0.0007;

    float row = floor(uv.y * LINES);

    // The OLD signal falls apart: hold degrades from fine tearing into a full roll.
    float chaosOld = smoothstep(0.0, 0.75, prog);

    // The NEW signal arrives broken and then LOCKS -- its chaos collapses over the last stretch.
    float chaosNew = 1.0 - smoothstep(0.55, 0.95, prog);

    float offOld = lineOffset(row, iTime + prog * 3.0, chaosOld, seed);
    float offNew = lineOffset(row, iTime + prog * 3.0, chaosNew, seed + 0.41);

    // fract() = the picture wraps. Rolling off one edge and back in the other is the signature of a
    // lost hold; clamping here would just smear the edge and look like a slide.
    vec3 imgOld = texture(iChannel0, vec2(fract(uv.x + offOld), uv.y)).xyz;
    vec3 imgNew = texture(iChannel1, vec2(fract(uv.x + offNew), uv.y)).xyz;

    // Hand over at peak chaos, while everything is torn: the cut is hidden inside the mess, so the
    // eye never sees a dissolve -- it sees a set losing one channel and finding another.
    float toNew = smoothstep(0.42, 0.62, prog);
    vec3 col = mix(imgOld, imgNew, toNew);

    // --- the tube itself ---
    float chaosPeak = max(chaosOld * (1.0 - toNew), chaosNew * toNew);

    // COLOUR BURST LOST. A set that cannot hold sync cannot recover the colour subcarrier either, so
    // it falls back to monochrome and only re-colours once it locks. Authentic -- and it does real
    // work: draining the colour at exactly the moment the channels swap is what HIDES the swap. Two
    // different pictures with different palettes cut together invisibly if both are grey and full of
    // snow; in colour, the cut is obvious no matter how badly torn the picture is.
    float lum = dot(col, vec3(0.299, 0.587, 0.114));
    col = mix(col, vec3(lum), chaosPeak * 0.92);

    // Snow: worst at peak chaos, gone once locked.
    float snow = texture(sampler_noise_lq, uv * 3.0 + vec2(iTime * 9.0, iTime * 13.0)).x - 0.5;
    col += snow * 0.45 * chaosPeak;

    // Sync flicker: brightness pumps as the line oscillator hunts.
    float pump = 1.0 + 0.25 * chaosPeak * sin(iTime * 47.0);
    col *= pump;

    // The bright roll bar drifting up the screen -- the other unmistakable CRT tell.
    float bar = fract(uv.y + iTime * 0.35);
    col *= 1.0 + 0.30 * chaosPeak * smoothstep(0.92, 1.0, bar);

    // Scanlines, only while the tube is misbehaving.
    col *= 1.0 - 0.18 * chaosPeak * (0.5 + 0.5 * sin(uv.y * LINES * 6.2831853));

    fragColor = vec4(col, 1.0);
}
