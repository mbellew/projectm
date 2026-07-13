#pragma once

#include "PresetState.hpp"

namespace libprojectM {
namespace MilkdropPreset {

class CustomStroke;

/**
 * @class StrokePerFrameContext
 * @brief Per-frame execution context and code for custom strokes (cubic-Bezier ribbons).
 *
 * Mirrors ShapePerFrameContext. The variables the code can write are the stroke's control net
 * (x0..y3), its width profile (w0/wmid/w1) and its colour ramp (r,g,b,a -> r2,g2,b2,a2).
 **/
class StrokePerFrameContext
{
public:
    StrokePerFrameContext(projectm_eval_mem_buffer gmegabuf, PRJM_EVAL_F (*globalRegisters)[100],
                          const Palette* palette, const Renderer::PoseState* pose);

    ~StrokePerFrameContext();

    /** @brief Registers the state variables in the expression evaluator context. */
    void RegisterBuiltinVariables();

    /** @brief Loads the current state values into the expression evaluator variables. */
    void LoadStateVariables(const PresetState& state, CustomStroke& stroke, int inst);

    /**
     * @brief Compiles and runs the stroke's init code.
     * @throws MilkdropCompileException Thrown if the code couldn't be compiled.
     */
    void EvaluateInitCode(const std::string& perFrameInitCode, const CustomStroke& stroke);

    /**
     * @brief Compiles the per-frame code and stores the handle.
     * @throws MilkdropCompileException Thrown if the code couldn't be compiled.
     */
    void CompilePerFrameCode(const std::string& perFrameCode, const CustomStroke& stroke);

    /** @brief Executes the per-frame code with the current state. */
    void ExecutePerFrameCode();

    projectm_eval_context* perFrameCodeContext{nullptr}; //!< The code runtime context.
    projectm_eval_code* perFrameCodeHandle{nullptr};     //!< The compiled per-frame code handle.

    // Read-only inputs.
    PRJM_EVAL_F* time{};
    PRJM_EVAL_F* fps{};
    PRJM_EVAL_F* frame{};
    PRJM_EVAL_F* progress{};
    PRJM_EVAL_F* q_vars[QVarCount]{};
    PRJM_EVAL_F* t_vars[TVarCount]{};
    PRJM_EVAL_F* bass{};
    PRJM_EVAL_F* mid{};
    PRJM_EVAL_F* treb{};
    PRJM_EVAL_F* bass_att{};
    PRJM_EVAL_F* mid_att{};
    PRJM_EVAL_F* treb_att{};
    PRJM_EVAL_F* instance{};
    PRJM_EVAL_F* num_inst{};

    // The cubic-Bezier control net. The curve passes through (x0,y0) and (x3,y3); (x1,y1) and
    // (x2,y2) pull it without being touched -- the "loose spline" the Burning Man logo is drawn with.
    PRJM_EVAL_F* x0{};
    PRJM_EVAL_F* y0{};
    PRJM_EVAL_F* x1{};
    PRJM_EVAL_F* y1{};
    PRJM_EVAL_F* x2{};
    PRJM_EVAL_F* y2{};
    PRJM_EVAL_F* x3{};
    PRJM_EVAL_F* y3{};

    // Width profile along the curve: a quadratic Bezier through w0 -> wmid -> w1. w0=w1=0 with
    // wmid>0 gives a stroke that tapers to points at both ends; all three equal = constant width.
    PRJM_EVAL_F* w0{};
    PRJM_EVAL_F* wmid{};
    PRJM_EVAL_F* w1{};

    // Colour ramp from the start of the stroke (r,g,b,a) to its end (r2,g2,b2,a2).
    PRJM_EVAL_F* r{};
    PRJM_EVAL_F* g{};
    PRJM_EVAL_F* b{};
    PRJM_EVAL_F* a{};
    PRJM_EVAL_F* r2{};
    PRJM_EVAL_F* g2{};
    PRJM_EVAL_F* b2{};
    PRJM_EVAL_F* a2{};

    PRJM_EVAL_F* additive{};
    PRJM_EVAL_F* segments{}; //!< Tessellation along the curve.
};

} // namespace MilkdropPreset
} // namespace libprojectM
