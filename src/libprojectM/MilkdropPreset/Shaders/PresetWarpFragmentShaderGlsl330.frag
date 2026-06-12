precision mediump float;

in vec4 frag_COLOR;
in vec4 frag_TEXCOORD0;
in vec2 frag_TEXCOORD1;

uniform sampler2D texture_sampler;

layout(location = 0) out vec4 color;
layout(location = 1) out vec2 texCoords;

void main() {
    // Main image. frag_COLOR.rgb is the exponential decay factor. Pure exponential decay only
    // asymptotes toward zero, so a sparse preset keeps a faint trace of the previous busy preset
    // for a very long time. Subtracting a small linear term as well (scaled by how slow the
    // exponential decay is, so fast-decay presets are barely affected) lets faint remnants reach
    // exactly zero. The divisor is the tuning knob: smaller = clears trails faster.
    float linearDecay = (1.0 - frag_COLOR.r) / 30.0;
    vec4 sampled = texture(texture_sampler, frag_TEXCOORD0.xy);
    color.rgb = max(frag_COLOR.rgb * sampled.rgb - linearDecay, vec3(0.0));
    color.a = frag_COLOR.a * sampled.a;
    // Motion vector grid u/v coords for the next frame
    texCoords = frag_TEXCOORD0.xy;
}
