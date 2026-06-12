#include "Renderer/ColorPalette.hpp"

#include <gtest/gtest.h>

#include <cmath>

using libprojectM::Renderer::ColorPalette;
using Family = ColorPalette::Family;

namespace {

auto to255(float channel) -> int
{
    return static_cast<int>(std::lround(channel * 255.0f));
}

struct Reference
{
    Family family;
    float knob;
    float t;
    int r;
    int g;
    int b;
};

} // namespace

/*
 * Reference values are produced by palette_proof.py, the canonical spec the
 * generator is ported from. They lock the generator to the validated curves so
 * future tweaks can't silently drift the colors (and, later, can't drift the
 * GPU LUT bake away from the expression-side accessors).
 */
TEST(ColorPalette, MatchesReferenceSpec)
{
    const Reference refs[] = {
        {Family::Pastel, 0.30f, 0.000f, 232, 220, 223},
        {Family::Pastel, 0.90f, 0.500f, 147, 233, 216},
        {Family::Muted, 0.60f, 0.250f, 146, 133, 100},
        {Family::Jewel, 0.90f, 0.400f, 22, 100, 28},
        {Family::Earth, 0.50f, 0.500f, 129, 106, 77},
        {Family::Monochromatic, 0.60f, 0.500f, 36, 110, 127},
        {Family::Neon, 0.90f, 0.300f, 253, 247, 24},
        {Family::SaturatedPrimary, 0.50f, 0.000f, 255, 12, 137},
        {Family::SaturatedPrimary, 0.50f, 0.666f, 0, 172, 251},
    };

    for (const auto& ref : refs)
    {
        const auto color = ColorPalette::ColorAt(ref.family, ref.knob, ref.t);
        // Allow +/-2 of 255 for double->float narrowing and rounding differences.
        EXPECT_NEAR(to255(color.r), ref.r, 2) << "family " << static_cast<int>(ref.family) << " R";
        EXPECT_NEAR(to255(color.g), ref.g, 2) << "family " << static_cast<int>(ref.family) << " G";
        EXPECT_NEAR(to255(color.b), ref.b, 2) << "family " << static_cast<int>(ref.family) << " B";
    }
}

TEST(ColorPalette, OutputsAreInUnitRange)
{
    for (int f = 0; f < static_cast<int>(Family::Count); ++f)
    {
        for (int ki = 0; ki <= 10; ++ki)
        {
            for (int ti = 0; ti <= 20; ++ti)
            {
                const auto color = ColorPalette::ColorAt(static_cast<Family>(f), ki / 10.0f, ti / 20.0f);
                for (float channel : {color.r, color.g, color.b})
                {
                    EXPECT_GE(channel, 0.0f);
                    EXPECT_LE(channel, 1.0f);
                }
            }
        }
    }
}

TEST(ColorPalette, HandleRoundTrips)
{
    for (int f = 0; f < static_cast<int>(Family::Count); ++f)
    {
        const auto family = static_cast<Family>(f);
        for (float knob : {0.0f, 0.25f, 0.5f, 0.73f, 0.999f})
        {
            const float handle = ColorPalette::PackHandle(family, knob);
            EXPECT_EQ(static_cast<int>(ColorPalette::FamilyOf(handle)), f);
            EXPECT_NEAR(ColorPalette::KnobOf(handle), knob, 1e-3f);
        }
    }
}

TEST(ColorPalette, HandleColorMatchesDirectColor)
{
    const float handle = ColorPalette::PackHandle(Family::Jewel, 0.6f);
    const auto viaHandle = ColorPalette::ColorAt(handle, 0.3f);
    const auto direct = ColorPalette::ColorAt(Family::Jewel, ColorPalette::KnobOf(handle), 0.3f);
    EXPECT_FLOAT_EQ(viaHandle.r, direct.r);
    EXPECT_FLOAT_EQ(viaHandle.g, direct.g);
    EXPECT_FLOAT_EQ(viaHandle.b, direct.b);
}
