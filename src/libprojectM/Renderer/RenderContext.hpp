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
};

} // namespace Renderer
} // namespace libprojectM
