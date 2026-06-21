/**
 * @file Palette.hpp
 * @brief A single preset-global color palette, resolved once at preset load and immutable after.
 *
 * The palette is declared in the preset header (PALETTE_NAME, PALETTE_SMOOTH[_H/_V]) rather than
 * through the expression evaluator, which has no string constants. It is resolved (name lookup,
 * `choose` random pick, smoothing) at parse time and never changes for the preset's lifetime;
 * `knob`/`t` are runtime sampling coordinates into that fixed palette.
 *
 * Stage 1 resolves a curated built-in family by name and samples it via the shared OKLab
 * generator. Image-file palettes + smoothing (the CPU pixel buffer that also feeds the GPU LUT)
 * land in a later stage; the smoothing radii are parsed and stored now so the API is stable.
 */
#pragma once

#include "Renderer/ColorPalette.hpp"
#include "Renderer/Texture.hpp"

#include <memory>
#include <string>

namespace libprojectM {
namespace MilkdropPreset {

class Palette
{
public:
    static constexpr int LutWidth = 256;  //!< LUT resolution along value (t / U).
    static constexpr int LutHeight = 64;  //!< LUT resolution along knob (V).

    /**
     * @brief Resolves the palette from the PALETTE_NAME spec + smoothing radii. Frozen afterwards.
     * @param nameSpec A built-in family name, or a comma-separated list from which one is chosen at
     *                 random (re-rolled each load). Empty / unknown falls back to a default family.
     * @param smoothH Horizontal (value-axis) smoothing radius. Stored now, applied once images land.
     * @param smoothV Vertical (knob-axis) smoothing radius.
     */
    void Resolve(const std::string& nameSpec, float smoothH, float smoothV);

    /**
     * @brief Samples the palette. @a knob = family tweak / image row (V), @a t = value (U). Clamped.
     * @return The sRGB color (each channel in [0,1]).
     */
    auto Sample(float knob, float t) const -> Renderer::ColorPalette::Rgb;

    /**
     * @brief Lazily bakes + returns the GPU LUT for shader sampling. Must be called on the GL
     * thread. U = value t, V = knob; CLAMP/LINEAR (via the fc_ sampler naming). The sRGB LUT
     * (RGBA8) is the display palette; the OKLab LUT (RGBA16F) drives perceptual snap/pull. Both
     * reproduce Sample() / the palette_r/g/b eval functions.
     */
    auto SrgbTexture() -> const std::shared_ptr<Renderer::Texture>&;
    auto LabTexture() -> const std::shared_ptr<Renderer::Texture>&;

private:
    Renderer::ColorPalette::Family m_family{Renderer::ColorPalette::Family::Pastel};
    float m_smoothH{0.0f};
    float m_smoothV{0.0f};

    std::shared_ptr<Renderer::Texture> m_srgbTexture; //!< Lazily baked 2D RGBA8 LUT (display).
    std::shared_ptr<Renderer::Texture> m_labTexture;  //!< Lazily baked 2D RGBA16F LUT (OKLab, snap/pull).
};

} // namespace MilkdropPreset
} // namespace libprojectM
