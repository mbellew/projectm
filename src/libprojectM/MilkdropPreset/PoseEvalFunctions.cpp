#include "PoseEvalFunctions.hpp"

#include "Renderer/RenderContext.hpp"

namespace libprojectM {
namespace MilkdropPreset {

namespace {

/* pose(joint, var): read one value of one joint from the smoothed skeleton
 * (user_data = const Renderer::PoseState*). Out-of-range indices return 0 rather than trapping, so
 * a preset can compute an index without guarding it. */
PRJM_EVAL_F Pose(void* user_data, int argc, PRJM_EVAL_F* argv)
{
    if (argc < 2 || user_data == nullptr)
    {
        return 0.0;
    }

    const auto* pose = static_cast<const Renderer::PoseState*>(user_data);
    const int joint = static_cast<int>(argv[0]);
    const int var = static_cast<int>(argv[1]);

    if (joint < 0 || joint >= Renderer::PoseJointCount || var < 0 || var >= Renderer::PoseVarCount)
    {
        return 0.0;
    }

    const Renderer::PoseJointState& j = pose->joints[joint];
    switch (var)
    {
        case Renderer::PoseVarX:
            return static_cast<PRJM_EVAL_F>(j.x);
        case Renderer::PoseVarY:
            return static_cast<PRJM_EVAL_F>(j.y);
        case Renderer::PoseVarZ:
            return static_cast<PRJM_EVAL_F>(j.z);
        case Renderer::PoseVarConf:
            return static_cast<PRJM_EVAL_F>(j.conf);
        case Renderer::PoseVarVx:
            return static_cast<PRJM_EVAL_F>(j.vx);
        case Renderer::PoseVarVy:
            return static_cast<PRJM_EVAL_F>(j.vy);
        default:
            return 0.0;
    }
}

//! Registers one ALL_CAPS constant. Phase A: a plain variable, written once and never updated.
//! (Phase B in POSE_API.md replaces this with real parser-side constants, which would also make
//! assigning to one a compile error instead of a silent foot-gun.)
void RegisterConstant(projectm_eval_context* context, const char* name, int value)
{
    PRJM_EVAL_F* slot = projectm_eval_context_register_variable(context, name);
    if (slot != nullptr)
    {
        *slot = static_cast<PRJM_EVAL_F>(value);
    }
}

} // namespace

void RegisterPoseFunctions(projectm_eval_context* context, const Renderer::PoseState* pose)
{
    // The eval API takes a non-const void*; the trampoline treats it as a const PoseState*.
    // Only the FUNCTION is registered here. The constants must be (re)set after every
    // reset_variables() -- see RegisterPoseConstants.
    void* const ud = const_cast<Renderer::PoseState*>(pose);
    projectm_eval_context_register_function(context, "pose", 2, &Pose, ud);
}

void RegisterPoseConstants(projectm_eval_context* context)
{
    // Joint names. Prefixed: the language is case-insensitive, so a bare NOSE/HEAD is fine but a
    // bare X or VX would collide with builtins and preset locals. Keep the whole set consistent.
    RegisterConstant(context, "JOINT_NOSE", Renderer::PoseJointNose);
    RegisterConstant(context, "JOINT_L_EYE", Renderer::PoseJointLEye);
    RegisterConstant(context, "JOINT_R_EYE", Renderer::PoseJointREye);
    RegisterConstant(context, "JOINT_L_EAR", Renderer::PoseJointLEar);
    RegisterConstant(context, "JOINT_R_EAR", Renderer::PoseJointREar);
    RegisterConstant(context, "JOINT_L_SHOULDER", Renderer::PoseJointLShoulder);
    RegisterConstant(context, "JOINT_R_SHOULDER", Renderer::PoseJointRShoulder);
    RegisterConstant(context, "JOINT_L_ELBOW", Renderer::PoseJointLElbow);
    RegisterConstant(context, "JOINT_R_ELBOW", Renderer::PoseJointRElbow);
    RegisterConstant(context, "JOINT_L_WRIST", Renderer::PoseJointLWrist);
    RegisterConstant(context, "JOINT_R_WRIST", Renderer::PoseJointRWrist);
    RegisterConstant(context, "JOINT_L_HIP", Renderer::PoseJointLHip);
    RegisterConstant(context, "JOINT_R_HIP", Renderer::PoseJointRHip);
    RegisterConstant(context, "JOINT_L_KNEE", Renderer::PoseJointLKnee);
    RegisterConstant(context, "JOINT_R_KNEE", Renderer::PoseJointRKnee);
    RegisterConstant(context, "JOINT_L_ANKLE", Renderer::PoseJointLAnkle);
    RegisterConstant(context, "JOINT_R_ANKLE", Renderer::PoseJointRAnkle);
    RegisterConstant(context, "JOINT_HEART", Renderer::PoseJointHeart);
    RegisterConstant(context, "JOINT_L_HAND", Renderer::PoseJointLHand);
    RegisterConstant(context, "JOINT_R_HAND", Renderer::PoseJointRHand);
    RegisterConstant(context, "JOINT_HEAD", Renderer::PoseJointHead);
    RegisterConstant(context, "JOINT_PELVIS", Renderer::PoseJointPelvis);
    RegisterConstant(context, "JOINT_COUNT", Renderer::PoseJointCount);

    // Which value of a joint to read.
    RegisterConstant(context, "POSE_X", Renderer::PoseVarX);
    RegisterConstant(context, "POSE_Y", Renderer::PoseVarY);
    RegisterConstant(context, "POSE_Z", Renderer::PoseVarZ);
    RegisterConstant(context, "POSE_CONF", Renderer::PoseVarConf);
    RegisterConstant(context, "POSE_VX", Renderer::PoseVarVx);
    RegisterConstant(context, "POSE_VY", Renderer::PoseVarVy);
}

} // namespace MilkdropPreset
} // namespace libprojectM
