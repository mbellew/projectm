#include "Palette.hpp"

#include "Utils.hpp"

#include <cstdint>
#include <cstdlib>
#include <sstream>
#include <vector>

namespace libprojectM {
namespace MilkdropPreset {

namespace {

using Family = Renderer::ColorPalette::Family;

/* Built-in family lookup by (already lower-cased) name. Returns Family::Count for unknown. */
auto FamilyByName(const std::string& name) -> Family
{
    if (name == "pastel") { return Family::Pastel; }
    if (name == "muted") { return Family::Muted; }
    if (name == "jewel") { return Family::Jewel; }
    if (name == "earth") { return Family::Earth; }
    if (name == "monochromatic" || name == "mono") { return Family::Monochromatic; }
    if (name == "neon") { return Family::Neon; }
    if (name == "saturated-primary" || name == "saturatedprimary" || name == "primary")
    {
        return Family::SaturatedPrimary;
    }
    return Family::Count;
}

auto Trim(const std::string& s) -> std::string
{
    const auto first = s.find_first_not_of(" \t\r\n");
    if (first == std::string::npos) { return {}; }
    const auto last = s.find_last_not_of(" \t\r\n");
    return s.substr(first, last - first + 1);
}

} // namespace

void Palette::Resolve(const std::string& nameSpec, float smoothH, float smoothV)
{
    m_smoothH = smoothH;
    m_smoothV = smoothV;

    // Split the comma-separated spec; an N-element list is a `choose` set (one picked at load).
    std::vector<std::string> names;
    std::stringstream stream(nameSpec);
    std::string item;
    while (std::getline(stream, item, ','))
    {
        const std::string trimmed = Trim(item);
        if (!trimmed.empty())
        {
            names.push_back(trimmed);
        }
    }

    if (names.empty())
    {
        m_family = Family::Pastel; // default when no PALETTE_NAME is declared
        return;
    }

    // Choose one uniformly at random (re-rolled each preset load), then freeze it.
    const std::string& pick = names[static_cast<size_t>(rand()) % names.size()];
    const Family family = FamilyByName(Utils::ToLower(pick));
    m_family = (family == Family::Count) ? Family::Pastel : family;
}

auto Palette::Sample(float knob, float t) const -> Renderer::ColorPalette::Rgb
{
    return Renderer::ColorPalette::ColorAt(m_family, knob, t);
}

namespace {
auto ToByte(float c) -> uint8_t
{
    const float v = c < 0.0f ? 0.0f : (c > 1.0f ? 1.0f : c);
    return static_cast<uint8_t>(v * 255.0f + 0.5f);
}
} // namespace

auto Palette::SrgbTexture() -> const std::shared_ptr<Renderer::Texture>&
{
    if (!m_srgbTexture)
    {
        // Bake the resolved palette into a 2D RGBA8 LUT: U = value t, V = knob, texel centers so a
        // shader sampling (t, knob) with linear filtering reproduces Sample().
        std::vector<uint8_t> rgba(static_cast<size_t>(LutWidth) * LutHeight * 4u);
        size_t idx = 0;
        for (int ky = 0; ky < LutHeight; ++ky)
        {
            const float knob = (static_cast<float>(ky) + 0.5f) / static_cast<float>(LutHeight);
            for (int tx = 0; tx < LutWidth; ++tx)
            {
                const float t = (static_cast<float>(tx) + 0.5f) / static_cast<float>(LutWidth);
                const auto color = Renderer::ColorPalette::ColorAt(m_family, knob, t);
                rgba[idx++] = ToByte(color.r);
                rgba[idx++] = ToByte(color.g);
                rgba[idx++] = ToByte(color.b);
                rgba[idx++] = 255;
            }
        }
        m_srgbTexture = std::make_shared<Renderer::Texture>(
            "palette", rgba.data(), GL_TEXTURE_2D, LutWidth, LutHeight, 1,
            GL_RGBA8, GL_RGBA, GL_UNSIGNED_BYTE, false);
    }
    return m_srgbTexture;
}

auto Palette::LabTexture() -> const std::shared_ptr<Renderer::Texture>&
{
    if (!m_labTexture)
    {
        // OKLab (L,a,b) sibling for perceptual snap/pull. RGBA16F (a/b are signed). Same layout.
        std::vector<float> data(static_cast<size_t>(LutWidth) * LutHeight * 4u);
        size_t idx = 0;
        for (int ky = 0; ky < LutHeight; ++ky)
        {
            const float knob = (static_cast<float>(ky) + 0.5f) / static_cast<float>(LutHeight);
            for (int tx = 0; tx < LutWidth; ++tx)
            {
                const float t = (static_cast<float>(tx) + 0.5f) / static_cast<float>(LutWidth);
                const auto lab = Renderer::ColorPalette::ColorAtLab(m_family, knob, t);
                data[idx++] = lab.L;
                data[idx++] = lab.a;
                data[idx++] = lab.b;
                data[idx++] = 0.0f;
            }
        }
        m_labTexture = std::make_shared<Renderer::Texture>(
            "palette_lab", data.data(), GL_TEXTURE_2D, LutWidth, LutHeight, 1,
            GL_RGBA16F, GL_RGBA, GL_FLOAT, false);
    }
    return m_labTexture;
}

} // namespace MilkdropPreset
} // namespace libprojectM
