#include "PaletteEvalFunctions.hpp"

#include "Renderer/ColorPalette.hpp"

#include <cstdint>

namespace libprojectM {
namespace MilkdropPreset {

namespace {

/* Shared callback for palette_r/g/b. The selected channel is encoded in user_data (0=r,1=g,2=b).
 * argv[0] = packed palette handle, argv[1] = position t along the palette curve. */
PRJM_EVAL_F PaletteChannel(void* user_data, int argc, PRJM_EVAL_F* argv)
{
    if (argc < 2)
    {
        return 0.0;
    }

    const auto color = Renderer::ColorPalette::ColorAt(static_cast<float>(argv[0]),
                                                       static_cast<float>(argv[1]));

    switch (reinterpret_cast<std::intptr_t>(user_data))
    {
        case 0:
            return color.r;
        case 1:
            return color.g;
        default:
            return color.b;
    }
}

} // namespace

void RegisterPaletteFunctions(projectm_eval_context* context)
{
    projectm_eval_context_register_function(context, "palette_r", 2, &PaletteChannel, reinterpret_cast<void*>(0));
    projectm_eval_context_register_function(context, "palette_g", 2, &PaletteChannel, reinterpret_cast<void*>(1));
    projectm_eval_context_register_function(context, "palette_b", 2, &PaletteChannel, reinterpret_cast<void*>(2));
}

} // namespace MilkdropPreset
} // namespace libprojectM
