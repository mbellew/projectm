#include "Renderer/ColorPalette.hpp"

#include <array>
#include <cmath>

namespace libprojectM {
namespace Renderer {

namespace {

constexpr double kPi = 3.14159265358979323846;

/* OKLab -> linear sRGB (Bjorn Ottosson). Computed in double; the final result is
 * narrowed to float on output. Mirrors palette_proof.py, the canonical spec. */
struct LinearRgb
{
    double r;
    double g;
    double b;
};

auto OkLabToLinearRgb(double L, double a, double b) -> LinearRgb
{
    const double l_ = L + 0.3963377774 * a + 0.2158037573 * b;
    const double m_ = L - 0.1055613458 * a - 0.0638541728 * b;
    const double s_ = L - 0.0894841775 * a - 1.2914855480 * b;
    const double l = l_ * l_ * l_;
    const double m = m_ * m_ * m_;
    const double s = s_ * s_ * s_;
    return {
        4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s,
        -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s,
        -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s};
}

auto LinearToSrgb(double c) -> double
{
    c = c < 0.0 ? 0.0 : (c > 1.0 ? 1.0 : c);
    return c <= 0.0031308 ? 12.92 * c : 1.055 * std::pow(c, 1.0 / 2.4) - 0.055;
}

auto InGamut(double L, double a, double b) -> bool
{
    constexpr double eps = 1e-4;
    const LinearRgb rgb = OkLabToLinearRgb(L, a, b);
    return rgb.r >= -eps && rgb.r <= 1.0 + eps &&
           rgb.g >= -eps && rgb.g <= 1.0 + eps &&
           rgb.b >= -eps && rgb.b <= 1.0 + eps;
}

/* Largest in-gamut chroma at a given lightness and hue, by binary search. */
auto MaxChroma(double L, double hueRadians) -> double
{
    const double cosH = std::cos(hueRadians);
    const double sinH = std::sin(hueRadians);
    double lo = 0.0;
    double hi = 0.5;
    for (int i = 0; i < 30; ++i)
    {
        const double mid = 0.5 * (lo + hi);
        if (InGamut(L, mid * cosH, mid * sinH))
        {
            lo = mid;
        }
        else
        {
            hi = mid;
        }
    }
    return lo;
}

/* Cusp lightness per integer hue: the lightness at which a hue reaches its
 * maximum chroma (= its most vivid in-gamut color). Built once, lazily. */
auto CuspLightness(int hueDegrees) -> double
{
    static const std::array<double, 360> table = [] {
        std::array<double, 360> values{};
        for (int h = 0; h < 360; ++h)
        {
            const double hueRadians = h * (kPi / 180.0);
            double bestL = 0.0;
            double bestC = 0.0;
            for (int i = 1; i < 100; ++i)
            {
                const double L = i / 100.0;
                const double c = MaxChroma(L, hueRadians);
                if (c > bestC)
                {
                    bestC = c;
                    bestL = L;
                }
            }
            values[h] = bestL;
        }
        return values;
    }();

    return table[((hueDegrees % 360) + 360) % 360];
}

auto Clamp01(float x) -> float
{
    return x < 0.0f ? 0.0f : (x > 1.0f ? 1.0f : x);
}

/* The family curves: (knob, t) -> (L, Cfrac, h in degrees) in OKLCh.
 * Final chroma is Cfrac * MaxChroma(L,h), keeping saturation gamut-even. */
struct Lch
{
    double L;
    double cFrac;
    double hueDegrees;
};

auto FamilyCurve(ColorPalette::Family family, double knob, double t) -> Lch
{
    switch (family)
    {
        case ColorPalette::Family::Pastel:
            return {0.92 - 0.05 * knob, 0.10 + 0.50 * knob, 360.0 * t};
        case ColorPalette::Family::Muted:
            return {0.62, 0.12 + 0.45 * knob, 360.0 * t};
        case ColorPalette::Family::Jewel:
            return {0.55 - 0.12 * knob, 0.45 + 0.50 * knob, 360.0 * t};
        case ColorPalette::Family::Earth:
            return {0.60 - 0.12 * t, 0.22 + 0.45 * knob, 35.0 + 75.0 * t};
        case ColorPalette::Family::Monochromatic:
            return {0.12 + 0.76 * t, 0.85 * std::sin(kPi * t), 360.0 * knob};
        case ColorPalette::Family::Neon:
            return {std::fmax(CuspLightness(static_cast<int>(std::lround(360.0 * t))), 0.80 - 0.12 * knob),
                    0.85 + 0.15 * knob, 360.0 * t};
        case ColorPalette::Family::SaturatedPrimary:
            return {CuspLightness(static_cast<int>(std::lround(360.0 * t))), 1.0, 360.0 * t};
        default:
            return {0.0, 0.0, 0.0};
    }
}

} // namespace

namespace {

/* The curve point as OKLab (L, a, b) — the form the families are actually defined in. */
auto CurveLab(ColorPalette::Family family, double knob, double t) -> ColorPalette::Lab
{
    const Lch lch = FamilyCurve(family, Clamp01(static_cast<float>(knob)), Clamp01(static_cast<float>(t)));
    const double hueRadians = lch.hueDegrees * (kPi / 180.0);
    const double chroma = lch.cFrac * MaxChroma(lch.L, hueRadians);
    return {static_cast<float>(lch.L),
            static_cast<float>(chroma * std::cos(hueRadians)),
            static_cast<float>(chroma * std::sin(hueRadians))};
}

} // namespace

auto ColorPalette::ColorAt(Family family, float knob, float t) -> Rgb
{
    const Lab lab = CurveLab(family, knob, t);
    const LinearRgb lin = OkLabToLinearRgb(lab.L, lab.a, lab.b);
    return {static_cast<float>(LinearToSrgb(lin.r)),
            static_cast<float>(LinearToSrgb(lin.g)),
            static_cast<float>(LinearToSrgb(lin.b))};
}

auto ColorPalette::ColorAtLab(Family family, float knob, float t) -> Lab
{
    return CurveLab(family, knob, t);
}

auto ColorPalette::PackHandle(Family family, float knob) -> float
{
    // Clamp knob just below 1.0 so it never carries into the next family's integer slot.
    constexpr float knobMax = 0.999985f; // ~1 - 1/65536
    const float clamped = knob < 0.0f ? 0.0f : (knob > knobMax ? knobMax : knob);
    return static_cast<float>(static_cast<int>(family)) + clamped;
}

auto ColorPalette::FamilyOf(float handle) -> Family
{
    const int index = static_cast<int>(std::floor(handle));
    if (index < 0 || index >= static_cast<int>(Family::Count))
    {
        return Family::Pastel;
    }
    return static_cast<Family>(index);
}

auto ColorPalette::KnobOf(float handle) -> float
{
    return handle - std::floor(handle);
}

auto ColorPalette::ColorAt(float handle, float t) -> Rgb
{
    return ColorAt(FamilyOf(handle), KnobOf(handle), t);
}

auto ColorPalette::BakeLut(int tSize, int knobSize) -> Lut
{
    if (tSize < 1) { tSize = 1; }
    if (knobSize < 1) { knobSize = 1; }

    const int depth = static_cast<int>(Family::Count);
    Lut lut;
    lut.width = tSize;
    lut.height = knobSize;
    lut.depth = depth;
    lut.rgba.resize(static_cast<size_t>(tSize) * static_cast<size_t>(knobSize) *
                    static_cast<size_t>(depth) * 4u);

    auto toByte = [](float c) -> uint8_t {
        const float v = c < 0.0f ? 0.0f : (c > 1.0f ? 1.0f : c);
        return static_cast<uint8_t>(v * 255.0f + 0.5f);
    };

    size_t idx = 0;
    for (int f = 0; f < depth; ++f)
    {
        for (int ky = 0; ky < knobSize; ++ky)
        {
            // Sample at texel centers so GPU linear filtering reproduces ColorAt() values.
            const float knob = (static_cast<float>(ky) + 0.5f) / static_cast<float>(knobSize);
            for (int tx = 0; tx < tSize; ++tx)
            {
                const float t = (static_cast<float>(tx) + 0.5f) / static_cast<float>(tSize);
                const Rgb c = ColorAt(static_cast<Family>(f), knob, t);
                lut.rgba[idx++] = toByte(c.r);
                lut.rgba[idx++] = toByte(c.g);
                lut.rgba[idx++] = toByte(c.b);
                lut.rgba[idx++] = 255;
            }
        }
    }
    return lut;
}

auto ColorPalette::BakeLutLab(int tSize, int knobSize) -> LabLut
{
    if (tSize < 1) { tSize = 1; }
    if (knobSize < 1) { knobSize = 1; }

    const int depth = static_cast<int>(Family::Count);
    LabLut lut;
    lut.width = tSize;
    lut.height = knobSize;
    lut.depth = depth;
    lut.data.resize(static_cast<size_t>(tSize) * static_cast<size_t>(knobSize) *
                    static_cast<size_t>(depth) * 4u);

    size_t idx = 0;
    for (int f = 0; f < depth; ++f)
    {
        for (int ky = 0; ky < knobSize; ++ky)
        {
            const float knob = (static_cast<float>(ky) + 0.5f) / static_cast<float>(knobSize);
            for (int tx = 0; tx < tSize; ++tx)
            {
                const float t = (static_cast<float>(tx) + 0.5f) / static_cast<float>(tSize);
                const Lab lab = ColorAtLab(static_cast<Family>(f), knob, t);
                lut.data[idx++] = lab.L;
                lut.data[idx++] = lab.a;
                lut.data[idx++] = lab.b;
                lut.data[idx++] = 0.0f;
            }
        }
    }
    return lut;
}

} // namespace Renderer
} // namespace libprojectM
