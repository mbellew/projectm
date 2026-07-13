#include "StrokePerFrameContext.hpp"

#include "CustomStroke.hpp"
#include "MilkdropPresetExceptions.hpp"
#include "PaletteEvalFunctions.hpp"
#include "PoseEvalFunctions.hpp"

#include <Logging.hpp>

#define REG_VAR(var) \
    var = projectm_eval_context_register_variable(perFrameCodeContext, #var);

namespace libprojectM {
namespace MilkdropPreset {

StrokePerFrameContext::StrokePerFrameContext(projectm_eval_mem_buffer gmegabuf,
                                             PRJM_EVAL_F (*globalRegisters)[100],
                                             const Palette* palette,
                                             const Renderer::PoseState* pose)
    : perFrameCodeContext(projectm_eval_context_create(gmegabuf, globalRegisters))
{
    RegisterPaletteFunctions(perFrameCodeContext, palette);
    RegisterPoseFunctions(perFrameCodeContext, pose);
}

StrokePerFrameContext::~StrokePerFrameContext()
{
    if (perFrameCodeHandle != nullptr)
    {
        projectm_eval_code_destroy(perFrameCodeHandle);
    }

    if (perFrameCodeContext != nullptr)
    {
        projectm_eval_context_destroy(perFrameCodeContext);
    }
}

void StrokePerFrameContext::RegisterBuiltinVariables()
{
    projectm_eval_context_reset_variables(perFrameCodeContext);
    // Constants must be re-set AFTER the reset: it zeroes every registered variable.
    RegisterPoseConstants(perFrameCodeContext);

    REG_VAR(time);
    REG_VAR(fps);
    REG_VAR(frame);
    REG_VAR(progress);

    for (int q = 0; q < QVarCount; q++)
    {
        std::string qvar = "q" + std::to_string(q + 1);
        q_vars[q] = projectm_eval_context_register_variable(perFrameCodeContext, qvar.c_str());
    }

    for (int t = 0; t < TVarCount; t++)
    {
        std::string tvar = "t" + std::to_string(t + 1);
        t_vars[t] = projectm_eval_context_register_variable(perFrameCodeContext, tvar.c_str());
    }

    REG_VAR(bass);
    REG_VAR(mid);
    REG_VAR(treb);
    REG_VAR(bass_att);
    REG_VAR(mid_att);
    REG_VAR(treb_att);
    REG_VAR(instance);
    REG_VAR(num_inst);

    REG_VAR(x0);
    REG_VAR(y0);
    REG_VAR(x1);
    REG_VAR(y1);
    REG_VAR(x2);
    REG_VAR(y2);
    REG_VAR(x3);
    REG_VAR(y3);

    REG_VAR(w0);
    REG_VAR(wmid);
    REG_VAR(w1);

    REG_VAR(r);
    REG_VAR(g);
    REG_VAR(b);
    REG_VAR(a);
    REG_VAR(r2);
    REG_VAR(g2);
    REG_VAR(b2);
    REG_VAR(a2);

    REG_VAR(additive);
    REG_VAR(segments);
}

void StrokePerFrameContext::LoadStateVariables(const PresetState& state, CustomStroke& stroke, int inst)
{
    *time = static_cast<double>(state.renderContext.time);
    *frame = static_cast<double>(state.renderContext.frame);
    *fps = static_cast<double>(state.renderContext.fps);
    *progress = static_cast<double>(state.renderContext.progress);
    *bass = static_cast<double>(state.audioData.bass);
    *mid = static_cast<double>(state.audioData.mid);
    *treb = static_cast<double>(state.audioData.treb);
    *bass_att = static_cast<double>(state.audioData.bassAtt);
    *mid_att = static_cast<double>(state.audioData.midAtt);
    *treb_att = static_cast<double>(state.audioData.trebAtt);

    for (int q = 0; q < QVarCount; q++)
    {
        *q_vars[q] = state.frameQVariables[q];
    }

    for (int t = 0; t < TVarCount; t++)
    {
        *t_vars[t] = stroke.m_tValuesAfterInitCode[t];
    }

    *instance = static_cast<double>(inst);
    *num_inst = static_cast<double>(stroke.m_instances);

    *x0 = static_cast<double>(stroke.m_x0);
    *y0 = static_cast<double>(stroke.m_y0);
    *x1 = static_cast<double>(stroke.m_x1);
    *y1 = static_cast<double>(stroke.m_y1);
    *x2 = static_cast<double>(stroke.m_x2);
    *y2 = static_cast<double>(stroke.m_y2);
    *x3 = static_cast<double>(stroke.m_x3);
    *y3 = static_cast<double>(stroke.m_y3);

    *w0 = static_cast<double>(stroke.m_w0);
    *wmid = static_cast<double>(stroke.m_wmid);
    *w1 = static_cast<double>(stroke.m_w1);

    *r = static_cast<double>(stroke.m_r);
    *g = static_cast<double>(stroke.m_g);
    *b = static_cast<double>(stroke.m_b);
    *a = static_cast<double>(stroke.m_a);
    *r2 = static_cast<double>(stroke.m_r2);
    *g2 = static_cast<double>(stroke.m_g2);
    *b2 = static_cast<double>(stroke.m_b2);
    *a2 = static_cast<double>(stroke.m_a2);

    *additive = static_cast<double>(stroke.m_additive);
    *segments = static_cast<double>(stroke.m_segments);
}

void StrokePerFrameContext::EvaluateInitCode(const std::string& perFrameInitCode, const CustomStroke& stroke)
{
    if (perFrameInitCode.empty())
    {
        return;
    }

    auto* initCode = projectm_eval_code_compile(perFrameCodeContext, perFrameInitCode.c_str());
    if (initCode == nullptr)
    {
        std::string error;
        int line;
        int col;
        auto* errmsg = projectm_eval_get_error(perFrameCodeContext, &line, &col);
        if (errmsg)
        {
            error = "[StrokePerFrameContext] Could not compile custom stroke ";
            error += std::to_string(stroke.m_index);
            error += " per-frame INIT code: ";
            error += std::string(errmsg);
            error += "(L" + std::to_string(line) + " C" + std::to_string(col) + ")";
        }
        else
        {
            error = "[StrokePerFrameContext] Could not compile custom stroke " +
                    std::to_string(stroke.m_index) + " per-frame init code.";
        }
        LOG_DEBUG("[StrokePerFrameContext] Failed custom stroke per-frame INIT code:\n" + perFrameInitCode);
        throw MilkdropCompileException(error);
    }

    projectm_eval_code_execute(initCode);
    projectm_eval_code_destroy(initCode);
}

void StrokePerFrameContext::CompilePerFrameCode(const std::string& perFrameCode, const CustomStroke& stroke)
{
    if (perFrameCode.empty())
    {
        return;
    }

    perFrameCodeHandle = projectm_eval_code_compile(perFrameCodeContext, perFrameCode.c_str());
    if (perFrameCodeHandle == nullptr)
    {
        std::string error;
        int line;
        int col;
        auto* errmsg = projectm_eval_get_error(perFrameCodeContext, &line, &col);
        if (errmsg)
        {
            error = "[StrokePerFrameContext] Could not compile custom stroke ";
            error += std::to_string(stroke.m_index);
            error += " per-frame code: ";
            error += errmsg;
            error += "(L" + std::to_string(line) + " C" + std::to_string(col) + ")";
        }
        else
        {
            error = "[StrokePerFrameContext] Could not compile custom stroke " +
                    std::to_string(stroke.m_index) + " per-frame code.";
        }
        LOG_DEBUG("[StrokePerFrameContext] Failed custom stroke per-frame code:\n" + perFrameCode);
        throw MilkdropCompileException(error);
    }
}

void StrokePerFrameContext::ExecutePerFrameCode()
{
    if (perFrameCodeHandle != nullptr)
    {
        projectm_eval_code_execute(perFrameCodeHandle);
    }
}

} // namespace MilkdropPreset
} // namespace libprojectM
