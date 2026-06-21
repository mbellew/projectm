#pragma once

#include <projectm-eval.h>

namespace libprojectM {
namespace MilkdropPreset {

class Palette;

/**
 * @brief Registers the curated-palette accessor functions on an expression-eval context.
 *
 * Adds the host functions:
 *   - palette_r(knob, t), palette_g(knob, t), palette_b(knob, t)
 * which return the red/green/blue channel (0..1) of the color at row @c knob and value position
 * @c t (both 0..1) of the preset's single resolved palette (declared via PALETTE_NAME). The palette
 * is fixed after parse; only knob/t are runtime coordinates.
 *
 * Must be called once per context, after creation and before compiling any code that uses them.
 * @param context The eval context to register the functions on.
 * @param palette  The preset's resolved palette (a stable pointer; sampled at call time). May point
 *                 at a not-yet-resolved palette at registration time — it's read lazily per call.
 */
void RegisterPaletteFunctions(projectm_eval_context* context, const Palette* palette);

} // namespace MilkdropPreset
} // namespace libprojectM
