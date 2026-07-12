#pragma once

#include <projectm-eval.h>

namespace libprojectM {

namespace Renderer {
struct PoseState;
}

namespace MilkdropPreset {

/**
 * @brief Registers the pose(JOINT, VAR) eval function in a context. Call once, at construction.
 *
 * Called from every eval context (per-frame, per-pixel, custom shape, custom wave per-frame and
 * per-point), exactly like RegisterPaletteFunctions -- which is what lets custom shapes read joints
 * directly, instead of bridging them through q-vars the way touch_* has to.
 *
 * @param context The eval context to register into.
 * @param pose The preset's pose state. Must outlive the context; contents update per frame.
 */
void RegisterPoseFunctions(projectm_eval_context* context, const Renderer::PoseState* pose);

/**
 * @brief Registers the ALL_CAPS pose constants (JOINT_*, POSE_*) and sets their values.
 *
 * MUST be called from RegisterBuiltinVariables(), AFTER projectm_eval_context_reset_variables():
 * that reset walks every registered variable and zeroes its value
 * (prjm_eval_reset_context_vars), so constants set at construction time would silently become 0 --
 * which makes pose(j, POSE_X) and pose(j, POSE_Y) both read var 0 and puts every joint on the
 * x==y diagonal.
 *
 * The constants are prefixed (JOINT_*, POSE_*) rather than bare (R_WRIST, X) because the eval
 * language is CASE-INSENSITIVE: a constant "X" would be the same identifier as the per-pixel mesh
 * coordinate "x", and "VX" would collide with preset locals. See POSE_API.md.
 */
void RegisterPoseConstants(projectm_eval_context* context);

} // namespace MilkdropPreset
} // namespace libprojectM
