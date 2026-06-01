/**
 * @file VideoPreprocessShaders.hpp
 * @brief GLSL sources for the GPU video-preprocessing pass.
 *
 * These compute the per-pixel alpha (mask) for a video frame on the GPU, replacing the
 * CPU ComputeAlpha path. The pass is a fullscreen triangle that samples the freshly
 * uploaded input frame plus the previous processed frame (and a background model for
 * BackgroundSubtract), and writes the processed RGBA. RGB passes through unchanged; only
 * alpha is synthesized, matching the modes in VideoTexture::AlphaMode.
 *
 * No "#version" line is included; the caller prepends the appropriate header
 * ("#version 330" on desktop, "#version 300 es" on GLES) like MilkdropStaticShaders.
 */
#pragma once

namespace libprojectM {
namespace Renderer {

//! Fullscreen-triangle vertex shader. Needs no vertex attributes (uses gl_VertexID),
//! but a VAO must still be bound when drawing under a core profile.
static constexpr const char* const kVideoPreprocessVertexShader = R"(
out vec2 v_uv;
void main()
{
    // Single triangle covering the viewport: positions (-1,-1),(3,-1),(-1,3),
    // texcoords (0,0),(2,0),(0,2).
    vec2 uv = vec2((gl_VertexID == 1) ? 2.0 : 0.0,
                   (gl_VertexID == 2) ? 2.0 : 0.0);
    v_uv = uv;
    gl_Position = vec4(uv * 2.0 - 1.0, 0.0, 1.0);
}
)";

//! Uber fragment shader. u_mode selects the AlphaMode (matching the enum values).
//! Output 0 = processed frame [rawRGB, alpha]; output 1 = updated background model.
static constexpr const char* const kVideoPreprocessFragmentShader = R"(
precision highp float;

in vec2 v_uv;

uniform sampler2D u_input; //!< Freshly uploaded (downscaled) camera frame.
uniform sampler2D u_prev;  //!< Previous processed frame: rgb = raw RGB, a = computed alpha.
uniform sampler2D u_bg;    //!< Background model (rgb), for BackgroundSubtract.

uniform int   u_mode;          //!< AlphaMode: 0 Source,1 Constant,2 Motion,3 MotionDecay,4 ChromaKey,5 BackgroundSubtract.
uniform float u_value;         //!< Motion/Decay scale, Constant alpha, ChromaKey tolerance, BgSubtract threshold.
uniform float u_init;          //!< Alpha for the very first frame (no history).
uniform float u_decay;         //!< MotionDecay persistence / BackgroundSubtract learning rate.
uniform vec3  u_key;           //!< ChromaKey background color (normalized).
uniform int   u_hasPrev;       //!< 0 on the first frame (no previous frame yet).
uniform int   u_hasBackground; //!< 0 until the background model has been seeded.
uniform int   u_mirror;        //!< Non-zero to horizontally mirror the incoming camera frame.

layout(location = 0) out vec4 o_frame; //!< Processed [rawRGB, alpha].
layout(location = 1) out vec4 o_bg;    //!< Updated background model.

const float INV_MAXDIST = 0.57735026; // 1/sqrt(3): max distance in normalized RGB space.
const float SOFT = 0.04;              // Feather band width for chroma/background thresholds.

float chebyshev(vec3 a, vec3 b)
{
    vec3 d = abs(a - b);
    return max(max(d.r, d.g), d.b);
}

float softGate(float dist, float threshold)
{
    float t = clamp((dist - threshold) / SOFT, 0.0, 1.0);
    return t * t * (3.0 - 2.0 * t); // smoothstep
}

void main()
{
    // Optionally mirror the incoming camera frame horizontally. Only the live input is flipped;
    // the already-processed prev/background frames are stored mirrored too, so motion/background
    // comparisons stay aligned.
    vec2 in_uv = (u_mirror != 0) ? vec2(1.0 - v_uv.x, v_uv.y) : v_uv;
    vec3 rgb  = texture(u_input, in_uv).rgb;
    float srcA = texture(u_input, in_uv).a;
    vec4 prev = texture(u_prev, v_uv);
    vec3 bg   = texture(u_bg, v_uv).rgb;

    float a = 1.0;
    vec3 bgOut = bg;

    if (u_hasPrev == 0)
    {
        a = clamp(u_init, 0.0, 1.0);
        if (u_mode == 5) { bgOut = rgb; } // seed background on first frame
    }
    else if (u_mode == 1) // Constant
    {
        a = clamp(u_value, 0.0, 1.0);
    }
    else if (u_mode == 2) // Motion
    {
        a = clamp(chebyshev(rgb, prev.rgb) * u_value, 0.0, 1.0);
    }
    else if (u_mode == 3) // MotionDecay
    {
        float motion = chebyshev(rgb, prev.rgb) * u_value;
        float decayed = prev.a * clamp(u_decay, 0.0, 1.0);
        a = clamp(max(motion, decayed), 0.0, 1.0);
    }
    else if (u_mode == 4) // ChromaKey
    {
        float dist = length(rgb - u_key) * INV_MAXDIST;
        a = softGate(dist, max(u_value, 0.0));
    }
    else if (u_mode == 5) // BackgroundSubtract
    {
        if (u_hasBackground == 0)
        {
            a = 0.0;
            bgOut = rgb;
        }
        else
        {
            // Gamma-expand before differencing: equal absolute RGB changes are
            // perceptually much larger in dark/shadowed regions, so sqrt() boosts
            // sensitivity there (gray/shadow foreground now registers).
            vec3 cur = sqrt(max(rgb, 0.0));
            vec3 ref = sqrt(max(bg, 0.0));
            float dist = length(cur - ref) * INV_MAXDIST;
            a = softGate(dist, max(u_value, 0.0));
            // Strong foreground bias: once a pixel reads foreground it persists so the
            // subject doesn't flicker or drop out. Exponential decay for a natural feel,
            // plus a small linear drain so the tail actually reaches zero instead of
            // asymptoting -- a faint geometric residual stays visible on BRIGHT
            // background (the comp shows rgb*alpha: dark residuals vanish, bright don't).
            const float FG_PERSIST = 0.85; // exponential factor
            const float FG_DRAIN = 0.02;   // linear push to zero (clears the tail)
            a = max(a, prev.a * FG_PERSIST - FG_DRAIN);
            // Dual-rate selective update: background adapts at u_decay; foreground barely
            // updates (FG_LEAK) so it isn't absorbed, while a frame-1 mis-seed still
            // self-corrects over time.
            const float FG_LEAK = 0.04;
            float rate = clamp(u_decay, 0.0, 1.0) * mix(FG_LEAK, 1.0, 1.0 - a);
            bgOut = mix(bg, rgb, rate);
        }
    }
    else // Source
    {
        a = srcA;
    }

    o_frame = vec4(rgb, a);
    o_bg = vec4(bgOut, 1.0);
}
)";

//! Morphology fragment shader for mask cleanup. Erode (u_op < 0 → 3x3 min) or dilate
//! (u_op > 0 → 3x3 max) the alpha channel; RGB is passed through from the center texel.
//! Open (erode then dilate) removes speckle; close (dilate then erode) fills pinholes.
static constexpr const char* const kVideoMorphFragmentShader = R"(
precision highp float;

in vec2 v_uv;

uniform sampler2D u_mask;  //!< Source mask: rgb = raw RGB, a = mask.
uniform vec2  u_texel;     //!< 1.0 / texture size.
uniform float u_op;        //!< < 0 = erode (min), > 0 = dilate (max).

layout(location = 0) out vec4 o_frame;

void main()
{
    vec4 c = texture(u_mask, v_uv);
    // Plus/cross structuring element (center + N/S/E/W, no diagonals). A 3x3 square
    // grows faster along diagonals (corners reach N*sqrt(2)), producing 45-degree
    // "ray" artifacts; the cross grows axis-symmetrically for a rounder halo.
    float n = texture(u_mask, v_uv + vec2(0.0, -1.0) * u_texel).a;
    float s = texture(u_mask, v_uv + vec2(0.0,  1.0) * u_texel).a;
    float e = texture(u_mask, v_uv + vec2( 1.0, 0.0) * u_texel).a;
    float w = texture(u_mask, v_uv + vec2(-1.0, 0.0) * u_texel).a;
    float a;
    if (u_op < 0.0) { a = min(min(min(min(c.a, n), s), e), w); } // erode
    else            { a = max(max(max(max(c.a, n), s), e), w); } // dilate
    o_frame = vec4(c.rgb, a);
}
)";

} // namespace Renderer
} // namespace libprojectM
