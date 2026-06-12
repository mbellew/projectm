#pragma once

#include <projectm-eval.h>

namespace libprojectM {
namespace MilkdropPreset {

/**
 * @brief Registers the curated-palette accessor functions on an expression-eval context.
 *
 * Adds the host functions:
 *   - palette_r(pal, t), palette_g(pal, t), palette_b(pal, t)
 * which return the red/green/blue channel (0..1) of the color at position @c t (0..1) along the
 * palette identified by the packed handle @c pal (see Renderer::ColorPalette). These are backed by
 * the single shared C++ generator, so colors match the GPU LUT bake.
 *
 * Must be called once per context, after creation and before compiling any code that uses them.
 * @param context The eval context to register the functions on.
 */
void RegisterPaletteFunctions(projectm_eval_context* context);

} // namespace MilkdropPreset
} // namespace libprojectM
