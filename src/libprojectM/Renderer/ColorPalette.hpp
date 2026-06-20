#pragma once

#include <cstdint>
#include <vector>

namespace libprojectM {
namespace Renderer {

/**
 * @brief Shared, single-source-of-truth generator for curated color palettes.
 *
 * Each palette @a Family is a smooth parametric curve through OKLCh, sampled by
 * a position @a t in [0,1]. A single @a knob in [0,1] tweaks the family; its
 * meaning is family-dependent (vividness/punch for the hue-sweep and arc
 * families, hue for Monochromatic). Internally a color is produced as
 * `(L, Cfrac, h)` in OKLCh where the final chroma is `Cfrac * max_chroma(L,h)`,
 * so saturation stays gamut-even across hue and never clips, then converted to
 * sRGB (Ottosson's OKLab matrices).
 *
 * This generator is the one place the curves are defined. The expression-side
 * accessors and the GPU LUT bake both consume it, guaranteeing CPU/GPU agreement.
 */
class ColorPalette
{
public:
    /** @brief An sRGB color, each channel in [0,1]. */
    struct Rgb
    {
        float r{};
        float g{};
        float b{};
    };

    /** @brief An OKLab color (perceptually uniform): L in [0,1], a/b roughly in [-0.4,0.4]. */
    struct Lab
    {
        float L{};
        float a{};
        float b{};
    };

    /** @brief Curated palette families. Integer values are part of the packed handle ABI. */
    enum class Family : int
    {
        Pastel = 0,
        Muted = 1,
        Jewel = 2,
        Earth = 3,
        Monochromatic = 4,
        Neon = 5,
        SaturatedPrimary = 6,

        Count
    };

    /**
     * @brief Samples a palette color.
     * @param family The palette family.
     * @param knob Family tweak in [0,1]: vividness for sweep/arc families, hue for Monochromatic.
     * @param t Position along the curve in [0,1] (clamped).
     * @return The sRGB color at that point on the curve.
     */
    static auto ColorAt(Family family, float knob, float t) -> Rgb;

    /**
     * @brief Packs a family + knob into a single float handle: `family + knob`
     * (integer part = family index, fractional part = knob). Exact in 32-bit float.
     * @param family The palette family.
     * @param knob Family tweak, clamped to [0,1) to avoid colliding with the next family.
     * @return The packed handle.
     */
    static auto PackHandle(Family family, float knob) -> float;

    /** @brief Extracts the family from a packed handle. */
    static auto FamilyOf(float handle) -> Family;

    /** @brief Extracts the knob from a packed handle. */
    static auto KnobOf(float handle) -> float;

    /** @brief Convenience: samples a color directly from a packed handle. */
    static auto ColorAt(float handle, float t) -> Rgb;

    /**
     * @brief Samples the palette color in OKLab (the space the curves are defined in).
     * Same point as ColorAt(), but returned as (L,a,b) without the sRGB conversion — used to
     * build the perceptual-distance LUT for shader snap/pull.
     */
    static auto ColorAtLab(Family family, float knob, float t) -> Lab;

    /** @brief A baked palette volume, ready to upload to a GL_TEXTURE_3D. */
    struct Lut
    {
        std::vector<uint8_t> rgba; //!< Tightly packed RGBA8, ordered [family][knob][t].
        int width{};               //!< t axis (texels).
        int height{};              //!< knob axis (texels).
        int depth{};               //!< family axis (= Family::Count).
    };

    /**
     * @brief Bakes every family into one RGBA8 volume for GPU sampling.
     *
     * Axis layout matches the shader sampler: x = @c t in [0,1], y = @c knob in [0,1],
     * z = family slice. Texels are sampled at their centers ((i+0.5)/size), so a shader
     * sampling at (t, knob, (family+0.5)/depth) with linear filtering reproduces ColorAt().
     * @param tSize    Resolution along t (e.g. 256).
     * @param knobSize Resolution along knob (e.g. 32).
     */
    static auto BakeLut(int tSize, int knobSize) -> Lut;

    /** @brief A baked OKLab palette volume, ready to upload to an RGBA16F GL_TEXTURE_3D. */
    struct LabLut
    {
        std::vector<float> data; //!< 4 floats per texel (L, a, b, 0), ordered [family][knob][t].
        int width{};
        int height{};
        int depth{};
    };

    /**
     * @brief Bakes every family into one OKLab (L,a,b) volume for perceptual shader matching.
     * Same axis layout / center convention as BakeLut(); used by snap/pull's distance + blend.
     */
    static auto BakeLutLab(int tSize, int knobSize) -> LabLut;
};

} // namespace Renderer
} // namespace libprojectM
