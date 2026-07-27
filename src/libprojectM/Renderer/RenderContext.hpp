/**
* @file RenderContext.hpp
* @brief A class holding per-frame render values for use in preset rendering.
*/
#pragma once

#include <projectM-4/projectM_cxx_export.h>

#include <string>
#include <vector>

namespace libprojectM {
namespace Renderer {

class ShaderCache;
class TextureManager;
class VideoTexture;

//! Joint indices. MUST match projectm_pose_joint_index in <projectM-4/pose.h>; these are the values
//! preset code sees as the ALL_CAPS constants (NOSE, R_WRIST, HEART, ...). See POSE_API.md.
enum PoseJoint : int
{
    PoseJointNose = 0,
    PoseJointLEye = 1,
    PoseJointREye = 2,
    PoseJointLEar = 3,
    PoseJointREar = 4,
    PoseJointLShoulder = 5,
    PoseJointRShoulder = 6,
    PoseJointLElbow = 7,
    PoseJointRElbow = 8,
    PoseJointLWrist = 9,
    PoseJointRWrist = 10,
    PoseJointLHip = 11,
    PoseJointRHip = 12,
    PoseJointLKnee = 13,
    PoseJointRKnee = 14,
    PoseJointLAnkle = 15,
    PoseJointRAnkle = 16,
    PoseJointHeart = 17,  //!< Chest: shoulder midpoint dropped toward the hips.
    PoseJointLHand = 18,  //!< Hand tip, extrapolated past the wrist (COCO-17 stops at the wrist).
    PoseJointRHand = 19,
    PoseJointHead = 20,   //!< Head center, above the nose.
    PoseJointPelvis = 21, //!< Hip midpoint.
    PoseJointLBreast = 22, //!< Upper-chest, bilinear in the shoulder-hip quad.
    PoseJointRBreast = 23,
    PoseJointNavel = 24,   //!< Lower torso, bilinear in the shoulder-hip quad.
    PoseJointGroin = 25,   //!< Hip midpoint extended below along the shoulder->hip axis.
    PoseJointThroat = 26,  //!< Shoulder midpoint raised toward the head.
    PoseJointCrown = 27,   //!< Top of head, above Head.
    PoseJointLFinger = 28, //!< Fingertip, extended further past the wrist than the Hand.
    PoseJointRFinger = 29,
    PoseJointLFoot = 30,   //!< Foot tip, extended past the ankle along the shin.
    PoseJointRFoot = 31,
    PoseJointCount = 32
};

//! Per-variable indices for the pose(JOINT, VAR) eval function (the ALL_CAPS X/Y/Z/CONF/VX/VY).
enum PoseVar : int
{
    PoseVarX = 0,
    PoseVarY = 1,
    PoseVarZ = 2,
    PoseVarConf = 3,
    PoseVarVx = 4,
    PoseVarVy = 5,
    PoseVarCount = 6
};

//! One smoothed joint. Position holds its last confident value while confidence decays, so a
//! preset anchoring to a joint doesn't get flung across the frame when the tracker drops it.
struct PoseJointState
{
    float x{0.5f};     //!< [0,1] left to right (mirror already applied).
    float y{0.5f};     //!< [0,1] bottom to top.
    float z{-1.0f};    //!< [0,1] closeness; <0 when no depth is available.
    float conf{0.0f};  //!< [0,1]; 0 = not detected.
    float vx{0.0f};    //!< Velocity X, screen-fractions/sec.
    float vy{0.0f};    //!< Velocity Y, screen-fractions/sec.
};

//! The full skeleton plus the derived scalars presets actually reach for. Read by the pose() eval
//! function (registered in every eval context) and by the pose_* per-frame variables.
struct PoseState
{
    PoseJointState joints[PoseJointCount]{};

    // Derived "robust primitives" -- these never misfire, unlike gesture recognition.
    float valid{0.0f};         //!< 1.0 when a person is tracked (pose_valid).
    float handsApart{0.0f};    //!< Distance between the hands, normalized (hands_apart).
    float handsTogether{0.0f}; //!< Smooth 1.0 as the hands close (hands_together).
    float handsHeight{0.0f};   //!< Mean hand height relative to the shoulders (hands_height).
    float armSpan{0.0f};       //!< Wrist-to-wrist distance (arm_span).
    float lunge{0.0f};         //!< Peak joint speed -- the impulse trigger (lunge).
};

/**
 * @brief Holds all global data of the current rendering context, which can change from frame to frame.
 */
class PROJECTM_CXX_EXPORT RenderContext
{
public:
    float time{0.0f};          //!< Time since the preset started, in seconds.
    int frame{0};              //!< Frames rendered so far.
    float fps{0.0f};           //!< Frames per second.
    float progress{0.0f};      //!< Preset progress.
    float blendProgress{0.0f}; //!< Preset transition/blending progress.
    int viewportSizeX{0};      //!< Horizontal viewport size in pixels
    int viewportSizeY{0};      //!< Vertical viewport size in pixels
    float aspectX{1.0};        //!< X aspect ratio.
    float aspectY{1.0};        //!< Y aspect ratio.
    float invAspectX{1.0};     //!< Inverse X aspect ratio.
    float invAspectY{1.0};     //!< Inverse Y aspect ratio.

    int perPixelMeshX{64}; //!< Per-pixel/per-vertex mesh X resolution.
    int perPixelMeshY{48}; //!< Per-pixel/per-vertex mesh Y resolution.

    float texelOffsetX{0.0f}; //!< Horizontal texel offset in the warp shader.
    float texelOffsetY{0.0f}; //!< Vertical texel offset in the warp shader.

    TextureManager* textureManager{nullptr}; //!< Holds all loaded textures for shader access.
    ShaderCache* shaderCache{nullptr}; //!< The shader chace of this projectM instance.

    //! Search paths for color-palette image files (PALETTE_NAME). Points at the projectM instance's
    //! list (stable; read only at preset load), so per-frame context copies stay cheap. Null = none.
    const std::vector<std::string>* paletteSearchPaths{nullptr};

    VideoTexture* videoTexture{nullptr}; //!< Optional 3D video-history texture, null if not configured.
    float videoZWrite{0.0f};             //!< Normalized Z of the most-recent video slice (uniform video_z_write).
    float videoZRange{0.0f};             //!< Normalized Z range of valid slices (uniform video_z_range).
    float videoFrameCount{0.0f};         //!< Number of video frames uploaded so far (uniform video_frame_count).
    float videoBufferSeconds{0.0f};      //!< Wall-clock seconds spanned by valid slices (uniform video_buffer_seconds).

    // Person-seg centroid, smoothed by the library and exposed to presets as seg_* eval
    // variables (and seg_* shader uniforms). Centered (0.5, 0.5) when no mask is present.
    float segCx{0.5f};       //!< Centroid X, [0,1] left to right (seg_cx).
    float segCy{0.5f};       //!< Centroid Y, [0,1] bottom to top (seg_cy).
    float segVx{0.0f};       //!< Centroid velocity X, screen-fractions/sec (seg_vx).
    float segVy{0.0f};       //!< Centroid velocity Y, screen-fractions/sec (seg_vy).
    float segCoverage{0.0f}; //!< Foreground fraction of the frame, [0,1] (seg_coverage).
    float segValid{0.0f};    //!< 1.0 when a confident mask is present, else 0.0 (seg_valid).

    // Main-figure exposure state from the NudeNet detector, already de-flickered by the app
    // (near-binary, covered/0 by default). Exposed to presets as the nude_* eval variables.
    float nudeTop{0.0f};      //!< Bare chest of the main figure, [0,1] (nude_top).
    float nudeRear{0.0f};     //!< Bare buttocks of the main figure, [0,1] (nude_rear).
    float nudeFrontF{0.0f};   //!< Exposed female genitalia, [0,1] (nude_front_f).
    float nudeFrontM{0.0f};   //!< Exposed male genitalia, [0,1] (nude_front_m).
    float nudeFemale{0.5f};   //!< Face gender axis: 1=female, 0=male, 0.5=unknown (nude_female).

    // Single arbitrated touch point, exposed to presets as touch_* eval variables. Written by
    // ProjectM::Touch/TouchDrag/TouchDestroy (mouse today, pose bridge later). Coordinate
    // convention matches seg_* exactly (Y bottom to top) so presets treat them interchangeably.
    float touchOn{0.0f};       //!< 1.0 while a touch is active, else 0.0 (touch_on).
    float touchX{0.5f};        //!< Touch X, [0,1] left to right (touch_x).
    float touchY{0.5f};        //!< Touch Y, [0,1] bottom to top (touch_y).
    float touchPressure{0.0f}; //!< Touch pressure, [0,1]; 0 if the source has no pressure axis (touch_pressure).
    float touchVx{0.0f};       //!< Touch X velocity, screen-fractions/sec (touch_vx).
    float touchVy{0.0f};       //!< Touch Y velocity, screen-fractions/sec (touch_vy).

    //! Full body skeleton, exposed to preset code via the pose(JOINT, VAR) eval function (in every
    //! eval context, including custom shapes) plus the pose_* derived scalars. See POSE_API.md.
    PoseState pose{};
};

} // namespace Renderer
} // namespace libprojectM
