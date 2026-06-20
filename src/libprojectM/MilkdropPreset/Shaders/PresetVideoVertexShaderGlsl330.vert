precision highp float;

// Fullscreen-triangle vertex shader for the preset video_ shader (the GPU video-preprocess
// combine pass). Sourced from the VideoTexture fullscreen-triangle VBO (a single vec2 position
// attribute at location 0). Emits frag_TEXCOORD0 — the varying name the HLSL->GLSL generator
// reads back for "_uv : TEXCOORD0" in the transpiled fragment shader.
layout(location = 0) in vec2 vertex_position;

out vec2 frag_TEXCOORD0;

void main()
{
    gl_Position = vec4(vertex_position, 0.0, 1.0);
    frag_TEXCOORD0 = vertex_position * 0.5 + 0.5;
}
