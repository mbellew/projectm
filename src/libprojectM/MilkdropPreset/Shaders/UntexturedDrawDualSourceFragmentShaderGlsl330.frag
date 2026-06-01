precision mediump float;

// FLOATBUF: dual-source output. Color attachment 0 receives (rgb, x): RGB is the draw color and x
// is the per-pixel state written into the pattern buffer's alpha channel. The second source (index 1)
// carries the draw alpha used only as the RGB blend weight, via:
//   glBlendFuncSeparate(SRC1_ALPHA, ONE_MINUS_SRC1_ALPHA,  <alpha factors>)
// so the RGB channels blend by the vertex alpha while the A channel is overwritten with x.

in vec4 fragment_color;
in float fragment_alpha_state;

layout(location = 0, index = 0) out vec4 outColor;
layout(location = 0, index = 1) out vec4 outBlendFactor;

void main(){
    outColor = vec4(fragment_color.rgb, fragment_alpha_state);
    outBlendFactor = vec4(fragment_color.a, fragment_color.a, fragment_color.a, fragment_color.a);
}
