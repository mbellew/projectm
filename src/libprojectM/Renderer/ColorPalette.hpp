#pragma once

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
};

} // namespace Renderer
} // namespace libprojectM
