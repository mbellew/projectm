#include "PerPixelContext.hpp"

#include "MilkdropPresetExceptions.hpp"
#include "PaletteEvalFunctions.hpp"
#include "PerFrameContext.hpp"

#include <Logging.hpp>

#define REG_VAR(var) \
    var = projectm_eval_context_register_variable(perPixelCodeContext, #var);

namespace libprojectM {
namespace MilkdropPreset {

PerPixelContext::PerPixelContext(projectm_eval_mem_buffer gmegabuf, PRJM_EVAL_F (*globalRegisters)[100], const Palette* palette)
    : perPixelCodeContext(projectm_eval_context_create(gmegabuf, globalRegisters))
{
    RegisterPaletteFunctions(perPixelCodeContext, palette);
}

PerPixelContext::~PerPixelContext()
{
    if (perPixelCodeHandle != nullptr)
    {
        projectm_eval_code_destroy(perPixelCodeHandle);
    }

    if (perPixelCodeContext != nullptr)
    {
        projectm_eval_context_destroy(perPixelCodeContext);
    }
}

void PerPixelContext::RegisterBuiltinVariables()
{
    projectm_eval_context_reset_variables(perPixelCodeContext);

    REG_VAR(zoom);
    REG_VAR(zoomexp);
    REG_VAR(rot);
    REG_VAR(warp);
    REG_VAR(cx);
    REG_VAR(cy);
    REG_VAR(dx);
    REG_VAR(dy);
    REG_VAR(sx);
    REG_VAR(sy);
    REG_VAR(time);
    REG_VAR(fps);
    REG_VAR(bass);
    REG_VAR(mid);
    REG_VAR(treb);
    REG_VAR(bass_att);
    REG_VAR(mid_att);
    REG_VAR(treb_att);
    REG_VAR(seg_cx);
    REG_VAR(seg_cy);
    REG_VAR(seg_vx);
    REG_VAR(seg_vy);
    REG_VAR(seg_coverage);
    REG_VAR(seg_valid);
    REG_VAR(frame);
    REG_VAR(x);
    REG_VAR(y);
    REG_VAR(rad);
    REG_VAR(ang);
    for (int q = 0; q < QVarCount; q++)
    {
        std::string qvar = "q" + std::to_string(q + 1);
        q_vars[q] = projectm_eval_context_register_variable(perPixelCodeContext, qvar.c_str());
    }
    REG_VAR(progress);
    REG_VAR(meshx);
    REG_VAR(meshy);
    REG_VAR(pixelsx);
    REG_VAR(pixelsy);
    REG_VAR(aspectx);
    REG_VAR(aspecty);
}

void PerPixelContext::LoadStateReadOnlyVariables(PresetState& state, PerFrameContext& perFrameState)
{
    *time = static_cast<PRJM_EVAL_F>(*perFrameState.time);
    *fps = static_cast<PRJM_EVAL_F>(*perFrameState.fps);
    *frame = static_cast<PRJM_EVAL_F>(*perFrameState.frame);
    *progress = static_cast<PRJM_EVAL_F>(*perFrameState.progress);
    *bass = static_cast<PRJM_EVAL_F>(*perFrameState.bass);
    *mid = static_cast<PRJM_EVAL_F>(*perFrameState.mid);
    *treb = static_cast<PRJM_EVAL_F>(*perFrameState.treb);
    *bass_att = static_cast<PRJM_EVAL_F>(*perFrameState.bass_att);
    *mid_att = static_cast<PRJM_EVAL_F>(*perFrameState.mid_att);
    *treb_att = static_cast<PRJM_EVAL_F>(*perFrameState.treb_att);
    *seg_cx = static_cast<PRJM_EVAL_F>(state.renderContext.segCx);
    *seg_cy = static_cast<PRJM_EVAL_F>(state.renderContext.segCy);
    *seg_vx = static_cast<PRJM_EVAL_F>(state.renderContext.segVx);
    *seg_vy = static_cast<PRJM_EVAL_F>(state.renderContext.segVy);
    *seg_coverage = static_cast<PRJM_EVAL_F>(state.renderContext.segCoverage);
    *seg_valid = static_cast<PRJM_EVAL_F>(state.renderContext.segValid);
    *meshx = static_cast<PRJM_EVAL_F>(state.renderContext.perPixelMeshX);
    *meshy = static_cast<PRJM_EVAL_F>(state.renderContext.perPixelMeshY);
    *pixelsx = static_cast<PRJM_EVAL_F>(state.renderContext.viewportSizeX);
    *pixelsy = static_cast<PRJM_EVAL_F>(state.renderContext.viewportSizeY);
    *aspectx = static_cast<PRJM_EVAL_F>(state.renderContext.aspectX);
    *aspecty = static_cast<PRJM_EVAL_F>(state.renderContext.aspectY);
}

void PerPixelContext::LoadPerFrameQVariables(PresetState& state, PerFrameContext& perFrameState)
{
    for (int q = 0; q < QVarCount; q++)
    {
        state.frameQVariables[q] = *perFrameState.q_vars[q];
        *q_vars[q] = *perFrameState.q_vars[q];
    }
}

void PerPixelContext::CopyPerFrameState(const PerPixelContext& source)
{
    *time = *source.time;
    *fps = *source.fps;
    *frame = *source.frame;
    *progress = *source.progress;
    *bass = *source.bass;
    *mid = *source.mid;
    *treb = *source.treb;
    *bass_att = *source.bass_att;
    *mid_att = *source.mid_att;
    *treb_att = *source.treb_att;
    *seg_cx = *source.seg_cx;
    *seg_cy = *source.seg_cy;
    *seg_vx = *source.seg_vx;
    *seg_vy = *source.seg_vy;
    *seg_coverage = *source.seg_coverage;
    *seg_valid = *source.seg_valid;
    *meshx = *source.meshx;
    *meshy = *source.meshy;
    *pixelsx = *source.pixelsx;
    *pixelsy = *source.pixelsy;
    *aspectx = *source.aspectx;
    *aspecty = *source.aspecty;
    for (int q = 0; q < QVarCount; q++)
    {
        *q_vars[q] = *source.q_vars[q];
    }
}

void PerPixelContext::CompilePerPixelCode(const std::string& perPixelCode)
{
    if (perPixelCode.empty())
    {
        return;
    }

    perPixelCodeHandle = projectm_eval_code_compile(perPixelCodeContext, perPixelCode.c_str());
    if (perPixelCodeHandle == nullptr)
    {
        std::string error;
        int line;
        int col;
        auto* errmsg = projectm_eval_get_error(perPixelCodeContext, &line, &col);
        if (errmsg)
        {
            error = "[PerPixelContext] Could not compile per-pixel code: ";
            error += errmsg;
            error += "(L" + std::to_string(line) + " C" + std::to_string(col) + ")";
        }
        else
        {
            error = "[PerPixelContext] Could not compile per-pixel code.";
        }
        LOG_DEBUG("[PerPixelContext] Failed per-pixel code:\n" + perPixelCode);
        throw MilkdropCompileException(error);
    }

    perPixelGlobalAccess = projectm_eval_code_global_access(perPixelCodeHandle);
}

void PerPixelContext::ExecutePerPixelCode()
{
    if (perPixelCodeHandle != nullptr)
    {
        projectm_eval_code_execute(perPixelCodeHandle);
    }
}

bool PerPixelContext::RequiresSerialEvaluation() const
{
    return (perPixelGlobalAccess & (PRJM_EVAL_ACCESS_MEGABUF |
                                    PRJM_EVAL_ACCESS_GMEGABUF |
                                    PRJM_EVAL_ACCESS_REGISTERS |
                                    PRJM_EVAL_ACCESS_RAND)) != 0;
}

} // namespace MilkdropPreset
} // namespace libprojectM
