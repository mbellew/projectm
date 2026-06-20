precision mediump float;

in vec2 fragment_texture;

uniform sampler2D texture_sampler;
uniform vec4 _c0; // source texsize (.xy), and inverse (.zw)
uniform vec4 _c5; // w1,w2,d1,d2
uniform vec4 _c6; // w_div, edge_darken_c1, edge_darken_c2, edge_darken_c3

out vec4 color;

void main(){
    // SHORT VERTICAL PASS 2:
    #define srctexsize _c0
    #define w1 _c5.x
    #define w2 _c5.y
    #define d1 _c5.z
    #define d2 _c5.w
    #define edge_darken_c1 _c6.y
    #define edge_darken_c2 _c6.z
    #define edge_darken_c3 _c6.w
    #define w_div   _c6.x

    // note: if you just take one sample at exactly uv.xy, you get an avg of 4
    // pixels.

    // + moves blur UP, LEFT by TWO-pixel increments! (since texture is 1/2 the
    // size of blur1_ps)
    vec2 uv2 = fragment_texture.xy + srctexsize.zw*vec2(0,0);

    // Blur all four channels (RGB + alpha) in one pass.
    vec4 blur =
    (texture(texture_sampler, uv2 + vec2(0, d1 * srctexsize.w)) +
    texture(texture_sampler, uv2 + vec2(0, -d1 * srctexsize.w))) *
    w1 +
    (texture(texture_sampler, uv2 + vec2(0, d2 * srctexsize.w)) +
    texture(texture_sampler, uv2 + vec2(0, -d2 * srctexsize.w))) *
    w2;
    blur *= w_div;

    // tone it down at the edges (only happens on 1st X pass!)
    float t = min(min(fragment_texture.x, fragment_texture.y),
    1.0 - max(fragment_texture.x, fragment_texture.y));
    t = sqrt(t);
    t = edge_darken_c1 + edge_darken_c2 * clamp(t * edge_darken_c3, 0.0, 1.0);
    // Edge-darken color only; keep alpha as the plain blurred value.
    color.xyz = blur.xyz * t;
    color.w = blur.w;
}