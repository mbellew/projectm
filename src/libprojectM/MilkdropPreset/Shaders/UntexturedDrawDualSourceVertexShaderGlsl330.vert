precision mediump float;

// FLOATBUF: dual-source variant of the untextured draw shader. Identical to the normal one, plus a
// per-vertex "alpha state" (x) passed through to the fragment shader to be written into the pattern
// buffer's alpha channel (the per-pixel state that follows the pixels through the warp feedback).

layout(location = 0) in vec2 vertex_position;
layout(location = 1) in vec4 vertex_color;
layout(location = 3) in float vertex_alpha_state;

uniform mat4 vertex_transformation;
uniform float vertex_point_size;

out vec4 fragment_color;
out float fragment_alpha_state;

void main(){
    gl_Position = vertex_transformation * vec4(vertex_position, 0.0, 1.0);
    gl_PointSize = vertex_point_size;
    fragment_color = vertex_color;
    fragment_alpha_state = vertex_alpha_state;
}
