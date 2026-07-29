#define  M_PI   3.14159265359
#define  M_PI_2 6.28318530718
#define  M_INV_PI_2  0.159154943091895

uniform float4   rand_frame;    // random float4, updated each frame
uniform float4   rand_preset;   // random float4, updated once per *preset*
uniform float4   _c0;           // .xy: multiplier to use on UV's to paste
                                // an image fullscreen, *aspect-aware*
                                // .zw = inverse.
uniform float4   _c1, _c2, _c3, _c4;
uniform float4   _c5;           // .xy = scale, bias for reading blur1
                                // .zw = scale, bias for reading blur2
uniform float4   _c6;           // .xy = scale, bias for reading blur3
                                // .zw = blur1_min, blur1_max
uniform float4   _c7;           // .xy ~= float2(1024,768)
                                // .zw ~= float2(1/1024.0, 1/768.0)
uniform float4   _c8;           // .xyzw ~= 0.5 + 0.5 * cos(
                                //   time * float4(~0.3, ~1.3, ~5, ~20))
uniform float4   _c9;           // .xyzw ~= same, but using sin()
uniform float4   _c10;          // .xyzw ~= 0.5 + 0.5 * cos(
                                //   time * float4(~0.005, ~0.008, ~0.013,
                                //                 ~0.022))
uniform float4   _c11;          // .xyzw ~= same, but using sin()
uniform float4   _c12;          // .xyz = mip info for main image
                                // (.x=#across, .y=#down, .z=avg)
                                // .w = unused
uniform float4   _c13;          // .xy = blur2_min, blur2_max
                                // .zw = blur3_min, blur3_max
uniform float4   _qa;           // q vars bank 1 [q1-q4]
uniform float4   _qb;           // q vars bank 2 [q5-q8]
uniform float4   _qc;           // q vars ...
uniform float4   _qd;           // q vars
uniform float4   _qe;           // q vars
uniform float4   _qf;           // q vars
uniform float4   _qg;           // q vars
uniform float4   _qh;           // q vars bank 8 [q29-q32]

// Video history texture state. Set by the library when a video texture has been
// configured via projectm_video_configure(). All zero when no video texture exists.
uniform float    video_z_write;       // normalized Z (slice center) of the most recent frame
uniform float    video_z_range;       // normalized Z range covered by valid (filled) slices
uniform float    video_frame_count;   // total frames uploaded so far (0 on first frame)
uniform float    video_buffer_seconds; // wall-clock seconds of history the valid slices span (0 until measured)

// Engine beat tracker (Audio/BeatDetect). Available directly in warp/comp shaders. Names are
// namespaced under beat_* to avoid colliding with preset-defined "beat"/"bpm" variables.
uniform float    beat_phase;          // phase-locked beat phase, 0 .. 2*pi (0 = on the beat)
uniform float    beat_onset;          // 1.0 on frames where a beat landed, else 0.0
uniform float    beat_bpm;            // estimated tempo (BPM); 0 until the tracker locks
uniform float    beat_conf;           // beat-tracker confidence, 0 .. 1

// Person-seg centroid, smoothed by the library (set via projectm_video_set_seg_centroid).
// Eases to screen center (0.5, 0.5) when no mask is present. seg_v* are screen-fractions/sec.
uniform float    seg_cx;              // centroid X, 0 .. 1 (left to right)
uniform float    seg_cy;              // centroid Y, 0 .. 1 (bottom to top)
uniform float    seg_vx;              // centroid velocity X
uniform float    seg_vy;              // centroid velocity Y
uniform float    seg_coverage;        // foreground fraction of the frame, 0 .. 1
uniform float    seg_idle;             // 1 = the "person" is a synthetic idle stand-in, not a real body
uniform float    seg_valid;           // 1.0 when a confident mask is present, else 0.0
uniform float    pose_enabled;        // 1.0 when a pose model is loaded and running
uniform float    nude_enabled;        // 1.0 when the NudeNet detector is loaded and running
uniform float    seg_enabled;         // 1.0 when person-segmentation masking is active

// note: in general, don't use the current time w/the *dynamic* rotations!

// four random, static rotations, randomized at preset load time.
// minor translation component (<1).
uniform float4x3 rot_s1;
uniform float4x3 rot_s2;
uniform float4x3 rot_s3;
uniform float4x3 rot_s4;

// four random, slowly changing rotations.
uniform float4x3 rot_d1;
uniform float4x3 rot_d2;
uniform float4x3 rot_d3;
uniform float4x3 rot_d4;

// faster-changing.
uniform float4x3 rot_f1;
uniform float4x3 rot_f2;
uniform float4x3 rot_f3;
uniform float4x3 rot_f4;

// very-fast-changing.
uniform float4x3 rot_vf1;
uniform float4x3 rot_vf2;
uniform float4x3 rot_vf3;
uniform float4x3 rot_vf4;

// ultra-fast-changing.
uniform float4x3 rot_uf1;
uniform float4x3 rot_uf2;
uniform float4x3 rot_uf3;
uniform float4x3 rot_uf4;

// Random every frame.
uniform float4x3 rot_rand1;
uniform float4x3 rot_rand2;
uniform float4x3 rot_rand3;
uniform float4x3 rot_rand4;

#define time     _c2.x
#define fps      _c2.y
#define frame    _c2.z
#define progress _c2.w
#define bass _c3.x
#define mid  _c3.y
#define treb _c3.z
#define vol  _c3.w
#define bass_att _c4.x
#define mid_att  _c4.y
#define treb_att _c4.z
#define vol_att  _c4.w
#define q1 _qa.x
#define q2 _qa.y
#define q3 _qa.z
#define q4 _qa.w
#define q5 _qb.x
#define q6 _qb.y
#define q7 _qb.z
#define q8 _qb.w
#define q9 _qc.x
#define q10 _qc.y
#define q11 _qc.z
#define q12 _qc.w
#define q13 _qd.x
#define q14 _qd.y
#define q15 _qd.z
#define q16 _qd.w
#define q17 _qe.x
#define q18 _qe.y
#define q19 _qe.z
#define q20 _qe.w
#define q21 _qf.x
#define q22 _qf.y
#define q23 _qf.z
#define q24 _qf.w
#define q25 _qg.x
#define q26 _qg.y
#define q27 _qg.z
#define q28 _qg.w
#define q29 _qh.x
#define q30 _qh.y
#define q31 _qh.z
#define q32 _qh.w

#define aspect   _c0

// .xy = (w,h); .zw = (1/(float)w, 1/(float)h)
#define texsize  _c7

#define roam_cos _c8
#define roam_sin _c9
#define slow_roam_cos _c10
#define slow_roam_sin _c11
#define mip_x   _c12.x
#define mip_y   _c12.y
#define mip_xy  _c12.xy
#define mip_avg _c12.z
#define blur1_min _c6.z
#define blur1_max _c6.w
#define blur2_min _c13.x
#define blur2_max _c13.y
#define blur3_min _c13.z
#define blur3_max _c13.w

#define sampler_FC_main sampler_fc_main
#define sampler_PC_main sampler_pc_main
#define sampler_FW_main sampler_fw_main
#define sampler_PW_main sampler_pw_main

#define GetMain(uv) (tex2D(sampler_main,uv).xyz)
#define GetPixel(uv) (tex2D(sampler_main,uv).xyz)
#define GetBlur1(uv) (tex2D(sampler_blur1,uv).xyz*_c5.x + _c5.y)
#define GetBlur2(uv) (tex2D(sampler_blur2,uv).xyz*_c5.z + _c5.w)
#define GetBlur3(uv) (tex2D(sampler_blur3,uv).xyz*_c6.x + _c6.y)

// Blurred alpha (per-pixel state). Unlike the color channels above, alpha is blurred
// in its native [0,1] range (no min/max renormalization), so it's read back directly.
#define GetBlurA1(uv) (tex2D(sampler_blur1,uv).a)
#define GetBlurA2(uv) (tex2D(sampler_blur2,uv).a)
#define GetBlurA3(uv) (tex2D(sampler_blur3,uv).a)

#define lum(x) (dot(x,float3(0.32,0.49,0.29)))
#define tex2d tex2D
#define tex3d tex3D

// Sample the video history texture.
// age: 0.0 = most recent frame, 1.0 = oldest available frame.
// Returns float4 (rgba) — alpha is preset-controlled via video_alpha_mode/value/init.
#define GetVideo(uv, age) tex3D(sampler_fw_video, float3(uv, frac(video_z_write - (age) * video_z_range + 1.0)))

// Current incoming (downscaled) video frame, video-space — the frame being processed THIS pass.
// Only meaningful inside a preset's video_ shader (the GPU preprocess combine pass): it pairs with
// GetVideo(uv, age) above (the history of past frames). rgb = the live frame, a = the alpha the
// preset's video_alpha_mode would have produced (the default the video_ shader can override).
#define GetVideoIn(uv) (tex2D(sampler_video_in, uv).xyz)
/*FLOATBUF*/ // Per-pixel state stored in the main buffer's alpha channel. Read with GetState(uv);
/*FLOATBUF*/ // write by assigning to the "ret_a" variable in a warp or comp shader_body.
#define GetState(uv) (tex2D(sampler_main, uv).a)

// Mask/analysis buffer (MASK_LAYERS.md): a current-frame, video-space float4 sibling of the
// video texture. R=seg (foreground/human matte), G=seg blur, B=motion, A=motion decay.
#define GetMask(uv) (tex2D(sampler_fc_mask, uv))
#define MaskSeg(uv) (tex2D(sampler_fc_mask, uv).r)
#define MaskSegBlur(uv) (tex2D(sampler_fc_mask, uv).g)
#define MaskMotion(uv) (tex2D(sampler_fc_mask, uv).b)
#define MaskMotionDecay(uv) (tex2D(sampler_fc_mask, uv).a)

// Curated color palette. The preset's single palette (PALETTE_NAME) is baked once into a 2D LUT
// and matches the per-frame palette_r/g/b(knob, t) eval functions (same baked data).
//   t    = value in [0,1], runs along the palette (U).
//   knob = the family tweak / image row in [0,1] (V); fixed or animated, the palette itself is not.
#define Palette(knob, t) (tex2D(sampler_fc_palette, float2(saturate(t), saturate(knob))).rgb)
// Gradient-map a color through the palette by its luminance (cheap duotone) at row knob. This is a
// luminance map; PaletteSnap/PalettePull below are the perceptual nearest-point versions.
#define PaletteMap(knob, c) (Palette(knob, lum(c)))

// Perceptual palette snap/pull. Both walk the palette's value axis at a fixed knob (PALETTE_STEPS
// taps) and project a color onto it. Distance AND (for pull) the blend happen in OKLab — the
// perceptually-uniform space the palettes are defined in — so matching tracks how the eye sees
// color and the soft blend stays vivid instead of desaturating toward gray. Samplers are always
// declared (see MilkdropShader); unused helper functions are dead-stripped by the GLSL compiler.
#define PALETTE_STEPS 24
#define PALETTE_INVSTEP (1.0 / 23.0) /* 1/(PALETTE_STEPS-1); keep in sync */

// Sample the palette in OKLab (L,a,b) at row knob — for matching, not display.
#define PaletteLab(knob, t) (tex2D(sampler_fc_palette_lab, float2(saturate(t), saturate(knob))).rgb)

// --- sRGB <-> OKLab (Bjorn Ottosson), matching ColorPalette.cpp. Gamma is piecewise but done
//     branchlessly (step+lerp); pow() bases are guarded >= 0 for out-of-[0,1] feedback colors. ---
float3 PaletteSrgbToLinear(float3 c)
{
    float3 lo = c / 12.92;
    float3 hi = pow(max((c + 0.055) / 1.055, float3(0.0, 0.0, 0.0)), float3(2.4, 2.4, 2.4));
    return lerp(lo, hi, step(float3(0.04045, 0.04045, 0.04045), c));
}
float3 PaletteLinearToSrgb(float3 c)
{
    c = saturate(c);
    float3 lo = c * 12.92;
    float3 hi = 1.055 * pow(c, float3(1.0 / 2.4, 1.0 / 2.4, 1.0 / 2.4)) - 0.055;
    return lerp(lo, hi, step(float3(0.0031308, 0.0031308, 0.0031308), c));
}
float3 PaletteSrgbToLab(float3 srgb)
{
    float3 c = PaletteSrgbToLinear(srgb);
    float l = dot(c, float3(0.4122214708, 0.5363325363, 0.0514459929));
    float m = dot(c, float3(0.2119034982, 0.6806995451, 0.1073969566));
    float s = dot(c, float3(0.0883024619, 0.2817188376, 0.6299787005));
    float l_ = pow(max(l, 0.0), 1.0 / 3.0);
    float m_ = pow(max(m, 0.0), 1.0 / 3.0);
    float s_ = pow(max(s, 0.0), 1.0 / 3.0);
    return float3(0.2104542553 * l_ + 0.7936177850 * m_ - 0.0040720468 * s_,
                  1.9779984951 * l_ - 2.4285922050 * m_ + 0.4505937099 * s_,
                  0.0259040371 * l_ + 0.7827717662 * m_ - 0.8086757660 * s_);
}
float3 PaletteLabToSrgb(float3 lab)
{
    float l_ = lab.x + 0.3963377774 * lab.y + 0.2158037573 * lab.z;
    float m_ = lab.x - 0.1055613458 * lab.y - 0.0638541728 * lab.z;
    float s_ = lab.x - 0.0894841775 * lab.y - 1.2914855480 * lab.z;
    float l = l_ * l_ * l_;
    float m = m_ * m_ * m_;
    float s = s_ * s_ * s_;
    float3 lin = float3( 4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s,
                        -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s,
                        -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s);
    return PaletteLinearToSrgb(lin);
}

// OKLab "distance", HUE-WEIGHTED: compare the chroma plane (a,b = hue+chroma) fully but lightness
// (L) only weakly, so a color matches the palette entry of the same hue regardless of how
// light/dark it is. PALETTE_LUMA_W = 1.0 = full perceptual (OKLab) distance; lower = more
// hue-driven. Shared by BOTH snap and pull.
#define PALETTE_LUMA_W 0.15
float PaletteDistLab(float3 labA, float3 labB)
{
    float dL = labA.x - labB.x;
    float2 dab = labA.yz - labB.yz;
    return dot(dab, dab) + PALETTE_LUMA_W * dL * dL;
}

// Nearest point on the palette (value axis, at row knob) to OKLab color cLab. Returns
// (lab = on-curve, a = distance).
float4 PaletteNearestLab(float knob, float3 cLab)
{
    float3 best = cLab;
    float bestd = 100.0; // larger than any PaletteDistLab
    for (int i = 0; i < PALETTE_STEPS; i++)
    {
        float3 pl = PaletteLab(knob, i * PALETTE_INVSTEP);
        float d = PaletteDistLab(cLab, pl);
        if (d < bestd) { bestd = d; best = pl; }
    }
    return float4(best, sqrt(bestd));
}

// Hard gradient-map: move c toward its nearest palette color (at row knob) by amount (0 = unchanged,
// 1 = on curve). Matching + blend are perceptual (OKLab); result converted back to sRGB once.
float3 PaletteSnap(float knob, float3 c, float amount)
{
    float3 cLab = PaletteSrgbToLab(c);
    float3 nLab = PaletteNearestLab(knob, cLab).rgb;
    return PaletteLabToSrgb(lerp(cLab, nLab, amount));
}

// Soft gradient-map: inverse-distance-weighted blend over the value axis (at row knob; no
// nearest-point pop), blended in OKLab so it stays vivid. sigma is the OKLab falloff width.
float3 PalettePull(float knob, float3 c, float amount, float sigma)
{
    float3 cLab = PaletteSrgbToLab(c);
    float3 num = float3(0.0, 0.0, 0.0);
    float den = 0.0;
    float inv = 1.0 / (2.0 * sigma * sigma);
    for (int i = 0; i < PALETTE_STEPS; i++)
    {
        float3 pl = PaletteLab(knob, i * PALETTE_INVSTEP);
        float w = exp(-PaletteDistLab(cLab, pl) * inv);
        num += w * pl;
        den += w;
    }
    return PaletteLabToSrgb(lerp(cLab, num / max(den, 1e-5), amount));
}
