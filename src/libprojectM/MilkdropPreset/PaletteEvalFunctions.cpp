#include "PaletteEvalFunctions.hpp"

#include "Palette.hpp"

namespace libprojectM {
namespace MilkdropPreset {

namespace {

/* palette_r/g/b(knob, t): sample the preset's resolved palette (user_data = const Palette*) at row
 * knob and value t. Three thin trampolines so the channel is the function, not encoded data. */
PRJM_EVAL_F PaletteRed(void* user_data, int argc, PRJM_EVAL_F* argv)
{
    if (argc < 2 || user_data == nullptr) { return 0.0; }
    return static_cast<const Palette*>(user_data)->Sample(static_cast<float>(argv[0]), static_cast<float>(argv[1])).r;
}

PRJM_EVAL_F PaletteGreen(void* user_data, int argc, PRJM_EVAL_F* argv)
{
    if (argc < 2 || user_data == nullptr) { return 0.0; }
    return static_cast<const Palette*>(user_data)->Sample(static_cast<float>(argv[0]), static_cast<float>(argv[1])).g;
}

PRJM_EVAL_F PaletteBlue(void* user_data, int argc, PRJM_EVAL_F* argv)
{
    if (argc < 2 || user_data == nullptr) { return 0.0; }
    return static_cast<const Palette*>(user_data)->Sample(static_cast<float>(argv[0]), static_cast<float>(argv[1])).b;
}

} // namespace

void RegisterPaletteFunctions(projectm_eval_context* context, const Palette* palette)
{
    // The eval API takes a non-const void*; the trampolines treat it as const Palette*.
    void* const ud = const_cast<Palette*>(palette);
    projectm_eval_context_register_function(context, "palette_r", 2, &PaletteRed, ud);
    projectm_eval_context_register_function(context, "palette_g", 2, &PaletteGreen, ud);
    projectm_eval_context_register_function(context, "palette_b", 2, &PaletteBlue, ud);
}

} // namespace MilkdropPreset
} // namespace libprojectM
