#pragma once

#include "PresetState.hpp"

#include <projectm-eval.h>

namespace libprojectM {
namespace MilkdropPreset {

class PerPixelContext
{
public:
    /**
     * @brief Constructor. Creates a new per-frame state object.
     * @param gmegabuf The global memory buffer to use in the code context.
     * @param globalRegisters The global variables to use in the code context.
     */
    PerPixelContext(projectm_eval_mem_buffer gmegabuf, PRJM_EVAL_F (*globalRegisters)[100], const Palette* palette, const Renderer::PoseState* pose);

    /**
     * @brief Destructor.
     */
    ~PerPixelContext();

    /**
     * @brief Registers the state variables in the expression evaluator context.
     */
    void RegisterBuiltinVariables();

    /**
     * @brief Loads the current state read-only values into the expression evaluator variables.
     *
     * All non-RO variables are loaded once per vertex. There's no real read-only flag, which means
     * preset authors may use this fact to change these values from vertex to vertex. Even if this
     * is an undocumented feature, we should do the same as some presets may depend on it.
     *
     * @param state The preset state container.
     * @param perFrameState The per-frame execution context.
     */
    void LoadStateReadOnlyVariables(PresetState& state, PerFrameContext& perFrameState);

    /**
     * @brief Copies the current per-frame Q variable values into the per-pixel state.
     * @param state The preset state container.
     * @param perFrameState The per-frame execution context.
     */
    void LoadPerFrameQVariables(PresetState& state, PerFrameContext& perFrameState);

    /**
     * @brief Compiles the per-pixel code and stores the code handle in the class.
     * @throws MilkdropCompileException Thrown if the per-pixel code couldn't be compiled.
     * @param perPixelCode The code to compile.
     */
    void CompilePerPixelCode(const std::string& perPixelCode);

    /**
     * @brief Copies the per-frame input variables from another context into this one.
     *
     * Used to prepare worker contexts for parallel per-pixel evaluation: it mirrors the values
     * set by LoadStateReadOnlyVariables() and LoadPerFrameQVariables() on the source context.
     * The per-vertex inputs (x/y/rad/ang) and the per-frame motion outputs (zoom..sy) are not
     * copied, as they are assigned fresh for every vertex during evaluation.
     * @param source The context to copy the per-frame state from.
     */
    void CopyPerFrameState(const PerPixelContext& source);

    /**
     * @brief Executes the per-pixel code with the current state.
     */
    void ExecutePerPixelCode();

    /**
     * @brief Returns whether the per-pixel code must be evaluated serially.
     *
     * True if the compiled code accesses any memory buffer (megabuf or gmegabuf), any of the
     * reg00-reg99 registers, or calls rand(). gmegabuf, the registers and rand()'s RNG state are
     * shared across contexts; the context-local megabuf is per-context but can carry state from
     * one vertex to the next, which parallel evaluation would not preserve. In all of these cases
     * the per-pixel loop cannot be safely or deterministically split across vertices.
     * @return True if the code requires serial evaluation, false otherwise (or if there is no code).
     */
    bool RequiresSerialEvaluation() const;

    projectm_eval_context* perPixelCodeContext{nullptr}; //!< The code runtime context, holds memory buffers and variables.
    projectm_eval_code* perPixelCodeHandle{nullptr};     //!< The compiled per-pixel code handle.
    int perPixelGlobalAccess{PRJM_EVAL_ACCESS_NONE};     //!< Bitmask of shared state accessed by the compiled code.

    PRJM_EVAL_F* zoom{};
    PRJM_EVAL_F* zoomexp{};
    PRJM_EVAL_F* rot{};
    PRJM_EVAL_F* warp{};
    PRJM_EVAL_F* cx{};
    PRJM_EVAL_F* cy{};
    PRJM_EVAL_F* dx{};
    PRJM_EVAL_F* dy{};
    PRJM_EVAL_F* sx{};
    PRJM_EVAL_F* sy{};
    PRJM_EVAL_F* time{};
    PRJM_EVAL_F* fps{};
    PRJM_EVAL_F* bass{};
    PRJM_EVAL_F* mid{};
    PRJM_EVAL_F* treb{};
    PRJM_EVAL_F* bass_att{};
    PRJM_EVAL_F* mid_att{};
    PRJM_EVAL_F* treb_att{};
    PRJM_EVAL_F* x{};
    PRJM_EVAL_F* y{};
    PRJM_EVAL_F* rad{};
    PRJM_EVAL_F* ang{};
    PRJM_EVAL_F* seg_cx{};       //!< Person-seg centroid X, 0..1 left to right.
    PRJM_EVAL_F* seg_cy{};       //!< Person-seg centroid Y, 0..1 bottom to top.
    PRJM_EVAL_F* seg_vx{};       //!< Person-seg centroid velocity X, screen-fractions/sec.
    PRJM_EVAL_F* seg_vy{};       //!< Person-seg centroid velocity Y, screen-fractions/sec.
    PRJM_EVAL_F* seg_coverage{}; //!< Person-seg foreground fraction, 0..1.
    PRJM_EVAL_F* seg_idle{};     //!< 1 when the matte is a synthetic idle stand-in, not a person.
    PRJM_EVAL_F* seg_valid{};    //!< 1.0 when a confident mask is present, else 0.0.
    PRJM_EVAL_F* nude_top{};      //!< Main figure bare chest, 0..1 (de-flickered; 0 = covered).
    PRJM_EVAL_F* nude_rear{};     //!< Main figure bare buttocks, 0..1.
    PRJM_EVAL_F* nude_front_f{};  //!< Main figure exposed female genitalia, 0..1.
    PRJM_EVAL_F* nude_front_m{};  //!< Main figure exposed male genitalia, 0..1.
    PRJM_EVAL_F* nude_female{};   //!< Face gender axis: 1=female, 0=male, 0.5=unknown.
    PRJM_EVAL_F* touch_on{};       //!< 1.0 while a touch is active, else 0.0.
    PRJM_EVAL_F* touch_x{};        //!< Touch X, 0..1 left to right.
    PRJM_EVAL_F* touch_y{};        //!< Touch Y, 0..1 bottom to top.
    PRJM_EVAL_F* touch_pressure{}; //!< Touch pressure, 0..1 (0 if the source has no pressure axis).
    PRJM_EVAL_F* touch_vx{};       //!< Touch X velocity, screen-fractions/sec.
    PRJM_EVAL_F* touch_vy{};       //!< Touch Y velocity, screen-fractions/sec.
    PRJM_EVAL_F* frame{};
    PRJM_EVAL_F* q_vars[QVarCount]{};
    PRJM_EVAL_F* progress{};
    PRJM_EVAL_F* meshx{};
    PRJM_EVAL_F* meshy{};
    PRJM_EVAL_F* pixelsx{};
    PRJM_EVAL_F* pixelsy{};
    PRJM_EVAL_F* aspectx{};
    PRJM_EVAL_F* aspecty{};
};

} // namespace MilkdropPreset
} // namespace libprojectM
