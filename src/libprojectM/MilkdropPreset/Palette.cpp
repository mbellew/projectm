#include "Palette.hpp"

#include "Utils.hpp"

#include <Logging.hpp>
#include <stb_image.h>

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <memory>
#include <sstream>
#include <vector>

namespace libprojectM {
namespace MilkdropPreset {

namespace {

using Family = Renderer::ColorPalette::Family;

constexpr int W = Palette::LutWidth;
constexpr int H = Palette::LutHeight;

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

auto Clamp01(float x) -> float { return x < 0.0f ? 0.0f : (x > 1.0f ? 1.0f : x); }

auto SrgbToLinear(float c) -> float
{
    return c <= 0.04045f ? c / 12.92f : std::pow((c + 0.055f) / 1.055f, 2.4f);
}
auto LinearToSrgb(float c) -> float
{
    c = Clamp01(c);
    return c <= 0.0031308f ? 12.92f * c : 1.055f * std::pow(c, 1.0f / 2.4f) - 0.055f;
}

/* sRGB -> OKLab (Ottosson), matching ColorPalette.cpp and the shader's PaletteSrgbToLab. */
void SrgbToOklab(float r, float g, float b, float& outL, float& outA, float& outB)
{
    const float lr = SrgbToLinear(r);
    const float lg = SrgbToLinear(g);
    const float lb = SrgbToLinear(b);
    const float l = 0.4122214708f * lr + 0.5363325363f * lg + 0.0514459929f * lb;
    const float m = 0.2119034982f * lr + 0.6806995451f * lg + 0.1073969566f * lb;
    const float s = 0.0883024619f * lr + 0.2817188376f * lg + 0.6299787005f * lb;
    const float l_ = std::cbrt(l);
    const float m_ = std::cbrt(m);
    const float s_ = std::cbrt(s);
    outL = 0.2104542553f * l_ + 0.7936177850f * m_ - 0.0040720468f * s_;
    outA = 1.9779984951f * l_ - 2.4285922050f * m_ + 0.4505937099f * s_;
    outB = 0.0259040371f * l_ + 0.7827717662f * m_ - 0.8086757660f * s_;
}

/* Separable Gaussian blur of an interleaved RGB float image, in LINEAR light, CLAMP edges.
 * sigmaX/sigmaY are in texels; an axis with sigma < 0.5 is left untouched. */
void BlurLinear(std::vector<float>& rgb, float sigmaX, float sigmaY)
{
    auto makeKernel = [](float sigma, std::vector<float>& k) {
        const int radius = std::max(1, static_cast<int>(std::ceil(3.0f * sigma)));
        k.assign(static_cast<size_t>(2 * radius + 1), 0.0f);
        float sum = 0.0f;
        for (int i = -radius; i <= radius; ++i)
        {
            const float w = std::exp(-static_cast<float>(i * i) / (2.0f * sigma * sigma));
            k[static_cast<size_t>(i + radius)] = w;
            sum += w;
        }
        for (auto& w : k) { w /= sum; }
        return radius;
    };

    // Work in linear light to avoid darkening.
    for (auto& c : rgb) { c = SrgbToLinear(c); }

    std::vector<float> tmp(rgb.size());

    if (sigmaX >= 0.5f)
    {
        std::vector<float> k;
        const int r = makeKernel(sigmaX, k);
        for (int y = 0; y < H; ++y)
        {
            for (int x = 0; x < W; ++x)
            {
                float acc[3] = {0.0f, 0.0f, 0.0f};
                for (int i = -r; i <= r; ++i)
                {
                    const int sx = std::min(W - 1, std::max(0, x + i));
                    const float w = k[static_cast<size_t>(i + r)];
                    const size_t si = (static_cast<size_t>(y) * W + sx) * 3;
                    acc[0] += w * rgb[si]; acc[1] += w * rgb[si + 1]; acc[2] += w * rgb[si + 2];
                }
                const size_t di = (static_cast<size_t>(y) * W + x) * 3;
                tmp[di] = acc[0]; tmp[di + 1] = acc[1]; tmp[di + 2] = acc[2];
            }
        }
        rgb.swap(tmp);
    }

    if (sigmaY >= 0.5f)
    {
        std::vector<float> k;
        const int r = makeKernel(sigmaY, k);
        for (int y = 0; y < H; ++y)
        {
            for (int x = 0; x < W; ++x)
            {
                float acc[3] = {0.0f, 0.0f, 0.0f};
                for (int i = -r; i <= r; ++i)
                {
                    const int sy = std::min(H - 1, std::max(0, y + i));
                    const float w = k[static_cast<size_t>(i + r)];
                    const size_t si = (static_cast<size_t>(sy) * W + x) * 3;
                    acc[0] += w * rgb[si]; acc[1] += w * rgb[si + 1]; acc[2] += w * rgb[si + 2];
                }
                const size_t di = (static_cast<size_t>(y) * W + x) * 3;
                tmp[di] = acc[0]; tmp[di + 1] = acc[1]; tmp[di + 2] = acc[2];
            }
        }
        rgb.swap(tmp);
    }

    for (auto& c : rgb) { c = LinearToSrgb(c); }
}

auto ToByte(float c) -> uint8_t
{
    const float v = Clamp01(c);
    return static_cast<uint8_t>(v * 255.0f + 0.5f);
}

/* Try to load a palette image "<name>.<ext>" from the search paths and resample it (bilinear) into
 * the W x H sRGB buffer: image U -> value t, image V -> knob. Returns true on the first match. */
auto LoadPaletteImage(const std::string& name, const std::vector<std::string>& searchPaths,
                      std::vector<float>& out) -> bool
{
    static const char* const extensions[] = {".png", ".jpg", ".jpeg", ".bmp", ".tga"};
    for (const auto& dir : searchPaths)
    {
        for (const char* ext : extensions)
        {
            std::string path = dir;
            if (!path.empty() && path.back() != '/') { path += '/'; }
            path += name + ext;

            int srcW = 0;
            int srcH = 0;
            int channels = 0;
            std::unique_ptr<stbi_uc, void (*)(void*)> img(
                stbi_load(path.c_str(), &srcW, &srcH, &channels, 4), stbi_image_free);
            if (img == nullptr || srcW < 1 || srcH < 1)
            {
                continue;
            }
            LOG_INFO("[Palette] loaded image palette: " + path);

            out.assign(static_cast<size_t>(W) * H * 3u, 0.0f);
            const stbi_uc* px = img.get();
            for (int y = 0; y < H; ++y)
            {
                float sy = ((static_cast<float>(y) + 0.5f) / static_cast<float>(H)) * static_cast<float>(srcH) - 0.5f;
                sy = std::min(static_cast<float>(srcH - 1), std::max(0.0f, sy));
                const int y0 = static_cast<int>(std::floor(sy));
                const int y1 = std::min(srcH - 1, y0 + 1);
                const float ay = sy - static_cast<float>(y0);
                for (int x = 0; x < W; ++x)
                {
                    float sx = ((static_cast<float>(x) + 0.5f) / static_cast<float>(W)) * static_cast<float>(srcW) - 0.5f;
                    sx = std::min(static_cast<float>(srcW - 1), std::max(0.0f, sx));
                    const int x0 = static_cast<int>(std::floor(sx));
                    const int x1 = std::min(srcW - 1, x0 + 1);
                    const float ax = sx - static_cast<float>(x0);
                    for (int c = 0; c < 3; ++c)
                    {
                        auto at = [&](int xx, int yy) {
                            return static_cast<float>(px[(static_cast<size_t>(yy) * srcW + xx) * 4 + c]) / 255.0f;
                        };
                        const float top = at(x0, y0) * (1.0f - ax) + at(x1, y0) * ax;
                        const float bot = at(x0, y1) * (1.0f - ax) + at(x1, y1) * ax;
                        out[(static_cast<size_t>(y) * W + x) * 3 + c] = top * (1.0f - ay) + bot * ay;
                    }
                }
            }
            return true;
        }
    }
    return false;
}

} // namespace

void Palette::Resolve(const std::string& nameSpec, float smoothH, float smoothV,
                      const std::vector<std::string>& searchPaths)
{
    m_smoothH = smoothH;
    m_smoothV = smoothV;
    m_srgbTexture.reset();
    m_labTexture.reset();

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

    // Choose one uniformly at random (re-rolled each preset load), then freeze it.
    const std::string pick = names.empty() ? std::string()
                                           : names[static_cast<size_t>(rand()) % names.size()];

    // Resolution order: an image file <pick>.<ext> on the palette search path wins; else a built-in
    // family of that name; else (empty/unknown) the default family. The image is the source of
    // truth for that case (decoded into m_srgb); built-ins bake from ColorPalette.
    bool loaded = false;
    if (!pick.empty())
    {
        loaded = LoadPaletteImage(pick, searchPaths, m_srgb);
    }
    if (!loaded)
    {
        Family family = Family::Pastel; // default
        if (!pick.empty())
        {
            const Family resolved = FamilyByName(Utils::ToLower(pick));
            if (resolved != Family::Count) { family = resolved; }
        }
        // Texel centers so a (t, knob) bilinear sample reproduces ColorAt() (modulo smoothing).
        m_srgb.assign(static_cast<size_t>(W) * H * 3u, 0.0f);
        size_t idx = 0;
        for (int y = 0; y < H; ++y)
        {
            const float knob = (static_cast<float>(y) + 0.5f) / static_cast<float>(H);
            for (int x = 0; x < W; ++x)
            {
                const float t = (static_cast<float>(x) + 0.5f) / static_cast<float>(W);
                const auto color = Renderer::ColorPalette::ColorAt(family, knob, t);
                m_srgb[idx++] = color.r;
                m_srgb[idx++] = color.g;
                m_srgb[idx++] = color.b;
            }
        }
    }

    // Smoothing radii are expressed as a fraction of each axis (PALETTE_SMOOTH ~ 0..1).
    BlurLinear(m_srgb, smoothH * static_cast<float>(W), smoothV * static_cast<float>(H));
}

auto Palette::Sample(float knob, float t) const -> Renderer::ColorPalette::Rgb
{
    if (m_srgb.empty())
    {
        return {};
    }
    // Match the GPU sampler: CLAMP_TO_EDGE + LINEAR, texel centers. Texture coord c maps to texel
    // position c*size - 0.5; clamp, then bilinear between the two bracketing texels.
    const float fx = Clamp01(t) * static_cast<float>(W) - 0.5f;
    const float fy = Clamp01(knob) * static_cast<float>(H) - 0.5f;
    const float cx = std::min(static_cast<float>(W - 1), std::max(0.0f, fx));
    const float cy = std::min(static_cast<float>(H - 1), std::max(0.0f, fy));
    const int x0 = static_cast<int>(std::floor(cx));
    const int y0 = static_cast<int>(std::floor(cy));
    const int x1 = std::min(W - 1, x0 + 1);
    const int y1 = std::min(H - 1, y0 + 1);
    const float ax = cx - static_cast<float>(x0);
    const float ay = cy - static_cast<float>(y0);

    auto at = [this](int x, int y, int ch) -> float {
        return m_srgb[(static_cast<size_t>(y) * W + x) * 3 + ch];
    };
    Renderer::ColorPalette::Rgb out{};
    float* o = &out.r;
    for (int ch = 0; ch < 3; ++ch)
    {
        const float top = at(x0, y0, ch) * (1.0f - ax) + at(x1, y0, ch) * ax;
        const float bot = at(x0, y1, ch) * (1.0f - ax) + at(x1, y1, ch) * ax;
        o[ch] = top * (1.0f - ay) + bot * ay;
    }
    return out;
}

auto Palette::SrgbTexture() -> const std::shared_ptr<Renderer::Texture>&
{
    if (!m_srgbTexture)
    {
        std::vector<uint8_t> rgba(static_cast<size_t>(W) * H * 4u);
        for (int i = 0; i < W * H; ++i)
        {
            rgba[static_cast<size_t>(i) * 4 + 0] = ToByte(m_srgb[static_cast<size_t>(i) * 3 + 0]);
            rgba[static_cast<size_t>(i) * 4 + 1] = ToByte(m_srgb[static_cast<size_t>(i) * 3 + 1]);
            rgba[static_cast<size_t>(i) * 4 + 2] = ToByte(m_srgb[static_cast<size_t>(i) * 3 + 2]);
            rgba[static_cast<size_t>(i) * 4 + 3] = 255;
        }
        m_srgbTexture = std::make_shared<Renderer::Texture>(
            "palette", rgba.data(), GL_TEXTURE_2D, W, H, 1,
            GL_RGBA8, GL_RGBA, GL_UNSIGNED_BYTE, false);
    }
    return m_srgbTexture;
}

auto Palette::LabTexture() -> const std::shared_ptr<Renderer::Texture>&
{
    if (!m_labTexture)
    {
        // OKLab derived from the (post-smoothing) sRGB buffer, so snap/pull match what's displayed.
        std::vector<float> data(static_cast<size_t>(W) * H * 4u);
        for (int i = 0; i < W * H; ++i)
        {
            float L = 0.0f;
            float a = 0.0f;
            float b = 0.0f;
            SrgbToOklab(m_srgb[static_cast<size_t>(i) * 3 + 0],
                        m_srgb[static_cast<size_t>(i) * 3 + 1],
                        m_srgb[static_cast<size_t>(i) * 3 + 2], L, a, b);
            data[static_cast<size_t>(i) * 4 + 0] = L;
            data[static_cast<size_t>(i) * 4 + 1] = a;
            data[static_cast<size_t>(i) * 4 + 2] = b;
            data[static_cast<size_t>(i) * 4 + 3] = 0.0f;
        }
        m_labTexture = std::make_shared<Renderer::Texture>(
            "palette_lab", data.data(), GL_TEXTURE_2D, W, H, 1,
            GL_RGBA16F, GL_RGBA, GL_FLOAT, false);
    }
    return m_labTexture;
}

} // namespace MilkdropPreset
} // namespace libprojectM
