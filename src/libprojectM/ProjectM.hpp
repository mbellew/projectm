/*
 * projectM -- Milkdrop-esque visualisation SDK
 * Copyright (C)2003-2007 projectM Team
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * See 'LICENSE.txt' included within this release
 *
 */
#pragma once

#include <projectM-4/projectM_cxx_export.h>

#include <Renderer/RenderContext.hpp>
#include <Renderer/TextureTypes.hpp>

#include <Audio/PCM.hpp>

#include <atomic>
#include <cstdint>
#include <istream>
#include <memory>
#include <string>
#include <vector>

namespace libprojectM {

namespace Renderer {
class CopyTexture;
class PresetTransition;
class Renderer;
class Texture;
class TextureManager;
class ShaderCache;
class TransitionShaderManager;
class VideoTexture;
} // namespace Renderer

namespace UserSprites {
class SpriteManager;
} // namespace UserSprites

class Preset;
class PresetFactoryManager;
class TimeKeeper;

class PROJECTM_CXX_EXPORT ProjectM
{
public:
    ProjectM();

    ProjectM(const ProjectM& other) = delete;
    ProjectM(ProjectM&& other) noexcept = delete;
    auto operator=(const ProjectM& other) -> ProjectM& = delete;
    auto operator=(ProjectM&& other) noexcept -> ProjectM& = delete;

    virtual ~ProjectM();

    /**
     * @brief Callback for notifying the integrating app that projectM wants to switch to a new preset.
     *
     * It is safe to call LoadPreset() from inside the callback. The app can decide when to actually
     * call the function or even ignore the request completely.
     *
     * @param isHardCut True if the switch event was caused by a hard cut, false if it is a soft cut.
     */
    virtual void PresetSwitchRequestedEvent(bool isHardCut) const;

    /**
     * @brief Callback for notifying the integrating app that the requested preset file couldn't be loaded.
     * @param presetFilename The filename of the preset that failed to load. Empty if loaded from a stream.
     * @param message The error message with the failure reason.
     */
    virtual void PresetSwitchFailedEvent(const std::string& presetFilename, const std::string& message) const;

    /**
     * @brief Loads the given preset file and performs a smooth or immediate transition.
     * @param presetFilename The preset filename to load.
     * @param smoothTransition If set to true, old and new presets will be blended over smoothly.
     *                         If set to false, the new preset will be rendered immediately.
     */
    void LoadPresetFile(const std::string& presetFilename, bool smoothTransition);

    /**
     * @brief Loads the given preset data and performs a smooth or immediate transition.
     *
     * This function assumes the data to be in Milkdrop format.
     *
     * @param presetData The preset data stream to load from.
     * @param smoothTransition If set to true, old and new presets will be blended over smoothly.
     *                         If set to false, the new preset will be rendered immediately.
     */
    void LoadPresetData(std::istream& presetData, bool smoothTransition);

    void SetWindowSize(uint32_t width, uint32_t height);

    /**
     * @brief Sets the texture paths used to find images for presets.
     *
     * Setting new texture paths will clear the texture manager cache and reload textures.
     * This can cause lags in rendering.
     *
     * @param texturePaths A list of paths projectM will look for texture images, in order.
     */
    void SetTexturePaths(std::vector<std::string> texturePaths);

    /** @brief Sets the search paths for color-palette image files (PALETTE_NAME). Read at preset load. */
    void SetPaletteSearchPaths(std::vector<std::string> paletteSearchPaths);

    /**
     * @brief Sets the paths searched for transition shaders, and reloads them.
     *
     * Transitions found here are used for the random transition pool, and can be looked up by name
     * (the filename without extension) -- which is how a preset will be able to ask for a specific
     * one. The compiled-in transitions remain as a fallback if these paths yield nothing.
     *
     * @param transitionSearchPaths Directories to scan for transition shaders.
     */
    void SetTransitionSearchPaths(std::vector<std::string> transitionSearchPaths);

    void ResetTextures();

    /**
     * @brief Sets a callback function for loading textures from non-filesystem sources.
     * @param callback The callback function, or nullptr to disable.
     */
    void SetTextureLoadCallback(Renderer::TextureLoadCallback callback);

    void RenderFrame(uint32_t targetFramebufferObject = 0);

    /**
     * @brief Sets a user-specified time for rendering the next frame
     * Negative values will make projectM use the system clock instead.
     * @param secondsSinceStart Fractional seconds since rendering the first frame.
     */
    void SetFrameTime(double secondsSinceStart);

    /**
     * @brief Gets the time of the last frame rendered.
     * @note This will not return the value set with SetFrameTime, but the actual time used to render the last frame.
     *       If a user-specified frame time was set, this value is returned. Otherwise, the frame time measured via the
     *       system clock will be returned.
     * @return Seconds elapsed rendering the last frame since starting projectM.
     */
    auto GetFrameTime() -> double;

    void SetBeatSensitivity(float sensitivity);

    auto GetBeatSensitivity() const -> float;

    auto SoftCutDuration() const -> double;

    void SetSoftCutDuration(double seconds);

    auto HardCutDuration() const -> double;

    void SetHardCutDuration(double seconds);

    auto HardCutEnabled() const -> bool;

    void SetHardCutEnabled(bool enabled);

    auto HardCutSensitivity() const -> float;

    void SetHardCutSensitivity(float sensitivity);

    /**
     * @brief Returns the currently set preset duration in seconds.
     * @return The currently set preset duration in seconds.
     */
    auto PresetDuration() const -> double;

    void SetPresetDuration(double seconds);

    /**
     * @brief Returns the current frames per second value.
     * @return The current frames per second value.
     */
    auto TargetFramesPerSecond() const -> int32_t;

    /**
     * @brief Sets a new current frames per second value.
     * @param fps The new frames per second value.
     */
    void SetTargetFramesPerSecond(int32_t fps);

    auto AspectCorrection() const -> bool;

    void SetAspectCorrection(bool enabled);

    auto EasterEgg() const -> float;

    void SetEasterEgg(float value);

    void MeshSize(uint32_t& meshResolutionX, uint32_t& meshResolutionY) const;

    void SetMeshSize(uint32_t meshResolutionX, uint32_t meshResolutionY);

    void TexelOffsets(float& texelOffsetX, float& texelOffsetY) const;

    void SetTexelOffsets(float texelOffsetX, float texelOffsetY);

    void Touch(float touchX, float touchY, int pressure, int touchType);

    void TouchDrag(float touchX, float touchY, int pressure);

    void TouchDestroy(float touchX, float touchY);

    void TouchDestroyAll();

    /// Turn on or off a lock that prevents projectM from switching to another preset
    void SetPresetLocked(bool locked);

    /// Returns true if the active preset is locked
    auto PresetLocked() const -> bool;

    /**
     * @brief Sets whether newly loaded presets should start with a clean (black) canvas.
     * @param enabled True to start with a clean canvas, false to copy the previous frame.
     */
    void SetPresetStartClean(bool enabled);

    /**
     * @brief Returns whether newly loaded presets start with a clean canvas.
     * @return True if presets start with a clean canvas.
     */
    auto PresetStartClean() const -> bool;

    auto PCM() -> Audio::PCM&;

    auto WindowWidth() -> int;

    auto WindowHeight() -> int;

    /**
     * @brief Creates a new user sprite.
     * @param type The type of the sprite to create.
     * @param spriteData The type-dependent sprite data.
     * @return A unique, non-zero sprite identifier if the sprite was created successfully,
     *         or zero if any error occurred or the sprite limit was reached.
     */
    auto AddUserSprite(const std::string& type, const std::string& spriteData) -> uint32_t;

    /**
     * @brief Destroys a single sprite using its identifier.
     * @param spriteIdentifier The identifier of the sprite to destroy.
     */
    void DestroyUserSprite(uint32_t spriteIdentifier);

    /**
     * @brief Destroys all active user sprites.
     */
    void DestroyAllUserSprites();

    /**
     * @brief Returns the current user sprite count.
     * @return The number of currently active user sprites.
     */
    auto UserSpriteCount() const -> uint32_t;

    /**
     * @brief Sets a new limit for user sprites.
     * Any sprites above the new limit will be removed in order, oldest first.
     * @param maxSprites The new sprite limit. 0 disables sprites, 16 is the default.
     */
    void SetUserSpriteLimit(uint32_t maxSprites);

    /**
     * @brief Returns the current user sprite display limit.
     * @return The current user sprite limit.
     */
    auto UserSpriteLimit() const -> uint32_t;

    /**
     * @brief Returns a list of identifiers for all active user sprites.
     * @return A list of all active user sprite identifiers, ordered by age (oldest first).
     */
    auto UserSpriteIdentifiers() const -> std::vector<uint32_t>;

    /**
     * @brief Allocates the video-history 3D texture and registers it with the texture manager.
     * @param width Width in pixels of each slice.
     * @param height Height in pixels of each slice.
     * @param depth Number of frames retained in the ring buffer.
     */
    void VideoConfigure(int width, int height, int depth);

    /**
     * @brief Submits a video frame to the history buffer. Safe to call from any thread.
     * @param data Pointer to tightly-packed pixel data.
     * @param srcWidth Source frame width in pixels.
     * @param srcHeight Source frame height in pixels.
     * @param format Pixel format (0=RGB, 1=RGBA, 2=BGRA).
     */
    void VideoSubmitFrame(const void* data, int srcWidth, int srcHeight, int format);

    /**
     * @brief GL texture name of the RGBA8 input surface, sized to the configured texture.
     * For applications that preprocess frames on the GPU. Returns 0 if not configured.
     * GL thread only. @see VideoSubmitFrameGPU.
     */
    auto VideoInputTexture() const -> unsigned int;

    /**
     * @brief Returns the GL texture id of the active preset's PRE-COMPOSITE image.
     *
     * This is the surface the composite shader reads, and the same image fed back as
     * "sampler_main" next frame -- i.e. what the warp shader actually drew, before the composite
     * stage crops/curves/shades/frames it. 0 if there is no active preset.
     */
    auto MainTexture() const -> unsigned int;

    /**
     * @brief Submits a frame the application has rendered into VideoInputTexture() on the GPU.
     * The frame is copied to the history buffer verbatim (alpha = app-supplied mask); preset
     * alpha modes and mask cleanup are bypassed. GL thread only.
     */
    void VideoSubmitFrameGPU();

    /**
     * @brief Sets the chroma-key background color (normalized 0..1) for the video
     * ChromaKey alpha mode. Supplied by the application (scene/camera dependent).
     */
    void VideoSetChromaKey(float r, float g, float b);

    /**
     * @brief Enables or disables horizontal mirroring of incoming video frames. Off by default.
     */
    void VideoSetMirror(bool mirror);

    /**
     * @brief Application-global foreground masking override. When mode >= 0 it overrides the
     * preset's alpha mode and runs the library's masking pipeline (optionally the refinement
     * back-end); mode < 0 leaves masking under preset control. See Renderer::VideoTexture::AlphaMode.
     */
    void VideoSetMaskMode(int mode, bool refine);

    /**
     * @brief Returns true if the video-history texture has been configured.
     */
    auto VideoIsActive() const -> bool;

    /**
     * @brief Sets the person-segmentation centroid for the current frame. Safe to call from any thread.
     * @param cx Centroid X in [0,1], left to right (camera-native orientation; mirror applied internally).
     * @param cy Centroid Y in [0,1], bottom to top (matches preset per-pixel y).
     * @param coverage Foreground fraction of the frame, [0,1]. Drives the confidence/return-to-center.
     */
    void VideoSetSegCentroid(float cx, float cy, float coverage);

    /**
     * @brief Submits the current body-pose skeleton. Safe to call from any thread.
     * @param jointsXYZC Flat array of 4 floats per joint (x, y, z, confidence), indexed by
     *        Renderer::PoseJoint. Camera-native X; the mirror is applied internally.
     * @param jointCount Number of joints supplied. Trailing joints are treated as not detected.
     */
    void SetPose(const float* jointsXYZC, size_t jointCount);

    /**
     * @brief Draws the given texture on the active preset's main texture to get a "burn-in" effect.
     * @param openGlTextureId The OpenGL texture to draw onto the active preset(s).
     * @param left Left coordinate in pixels on the destination texture.
     * @param top Top coordinate in pixels on the destination texture.
     * @param width Width of the final image on the destination texture in pixels, can be negative to flip it horizontally.
     * @param height Height of the final image on the destination texture in pixels, can be negative to flip it vertically.
     */
    void BurnInTexture(uint32_t openGlTextureId, int left, int top, int width, int height);

private:
    void Initialize();

    void CheckGLSLVersion();

    void StartPresetTransition(std::unique_ptr<Preset>&& preset, bool hardCut);

    void LoadIdlePreset();

    auto GetRenderContext() -> Renderer::RenderContext;

    //! Advances the smoothed person-seg centroid one frame (called from RenderFrame).
    void UpdateSegState(double dtSeconds);

    //! Finite-differences the touch point's velocity one frame (called from RenderFrame).
    void UpdateTouchState(double dtSeconds);

    //! Smooths the pose skeleton, derives per-joint velocity and the pose_* scalars (RenderFrame).
    void UpdatePoseState(double dtSeconds);

    uint32_t m_meshX{32};            //!< Per-point mesh horizontal resolution.
    uint32_t m_meshY{24};            //!< Per-point mesh vertical resolution.
    uint32_t m_targetFps{35};        //!< Target frames per second.
    uint32_t m_windowWidth{0};       //!< EvaluateFrameData window width. If 0, nothing is rendered.
    uint32_t m_windowHeight{0};      //!< EvaluateFrameData window height. If 0, nothing is rendered.
    double m_presetDuration{30.0};   //!< Preset duration in seconds.
    double m_softCutDuration{3.0};   //!< Soft cut transition time.
    double m_hardCutDuration{20.0};  //!< Time after which a hard cut can happen at the earliest.
    bool m_hardCutEnabled{false};    //!< If true, hard cuts based on beat detection are enabled.
    float m_hardCutSensitivity{2.0}; //!< Loudness sensitivity value for hard cuts.
    float m_beatSensitivity{1.0};    //!< General beat sensitivity modifier for presets.
    bool m_aspectCorrection{true};   //!< If true, corrects aspect ratio for non-rectangular windows.
    float m_easterEgg{1.0};          //!< Random preset duration modifier. See TimeKeeper class.
    float m_previousFrameVolume{};   //!< Volume in previous frame, used for hard cuts.
    float m_texelOffsetX{0.0};       //!< Horizontal warp shader texel offset
    float m_texelOffsetY{0.0};       //!< Vertical warp shader texel offset

    std::vector<std::string> m_textureSearchPaths;     ///!< List of paths to search for texture files
    std::vector<std::string> m_paletteSearchPaths;     ///!< Search paths for color-palette image files (PALETTE_NAME).
    std::vector<std::string> m_transitionSearchPaths;  ///!< Search paths for transition shader files.
    Renderer::TextureLoadCallback m_textureLoadCallback; //!< Optional callback for loading textures from non-filesystem sources.

    /** Timing information */
    int m_frameCount{0}; //!< Rendered frame count since start

    bool m_presetLocked{false};         //!< If true, the preset change event will not be sent.
    bool m_presetChangeNotified{false}; //!< Stores whether the user has been notified that projectM wants to switch the preset.
    bool m_presetStartClean{false};     //!< If true, new presets start with a black canvas instead of the previous frame.

    std::unique_ptr<PresetFactoryManager> m_presetFactoryManager; //!< Provides access to all available preset factories.

    Audio::PCM m_audioStorage;                                                    //!< Audio data buffer and analyzer instance.
    std::unique_ptr<Renderer::TextureManager> m_textureManager;                   //!< The texture manager.
    std::unique_ptr<Renderer::ShaderCache> m_shaderCache;                         //!< The global shader cache.
    std::unique_ptr<Renderer::TransitionShaderManager> m_transitionShaderManager; //!< The transition shader manager.
    std::unique_ptr<Renderer::CopyTexture> m_textureCopier;                       //!< Class that copies textures 1:1 to another texture or framebuffer.
    std::unique_ptr<Preset> m_activePreset;                                       //!< Currently loaded preset.
    std::unique_ptr<Preset> m_transitioningPreset;                                //!< Destination preset when smooth preset switching.
    std::unique_ptr<Renderer::PresetTransition> m_transition;                     //!< Transition effect used for blending.
    std::unique_ptr<TimeKeeper> m_timeKeeper;                                     //!< Keeps the different timers used to render and switch presets.
    std::unique_ptr<UserSprites::SpriteManager> m_spriteManager;                  //!< Manages all types of user sprites.
    std::unique_ptr<Renderer::VideoTexture> m_videoTexture;                       //!< 3D ring-buffer texture for app-provided video frames.
    // App-global video settings persist here (not just on the texture) so they
    // survive (re)creation of m_videoTexture in VideoConfigure, independent of
    // the order in which the app calls configure vs. the setters.
    int m_videoMaskMode{-1};      //!< App mask-mode override (-1 = preset-controlled).
    bool m_videoMaskRefine{false};
    bool m_videoMirror{false};
    float m_videoKeyR{0.0f}, m_videoKeyG{0.0f}, m_videoKeyB{0.0f};

    // Person-seg centroid. The app writes the measured values (capture thread) via
    // VideoSetSegCentroid; UpdateSegState smooths them once per frame (render thread) into the
    // seg_* outputs exposed to presets through RenderContext. See UpdateSegState for the model.
    std::atomic<uint32_t> m_segSeq{0};            //!< Bumped on each app update; render thread detects freshness.
    uint32_t m_segSeqSeen{0};                     //!< Last sequence the render thread processed.
    float m_segMeasuredCx{0.5f};                  //!< Latest app-provided centroid X (mirror-corrected).
    float m_segMeasuredCy{0.5f};                  //!< Latest app-provided centroid Y.
    float m_segMeasuredCoverage{0.0f};            //!< Latest app-provided foreground fraction.
    float m_segSecondsSinceUpdate{1.0e3f};        //!< Time since the last app update (staleness).
    float m_segCx{0.5f};                          //!< Smoothed centroid X (seg_cx).
    float m_segCy{0.5f};                          //!< Smoothed centroid Y (seg_cy).
    float m_segVx{0.0f};                          //!< Smoothed centroid velocity X (seg_vx), /sec.
    float m_segVy{0.0f};                          //!< Smoothed centroid velocity Y (seg_vy), /sec.
    float m_segCoverage{0.0f};                    //!< Smoothed foreground fraction (seg_coverage).
    float m_segValid{0.0f};                       //!< 1.0 when a confident mask is present (seg_valid).

    // Single arbitrated touch point. The app writes the measured values via Touch/TouchDrag/
    // TouchDestroy (mouse handler today, pose bridge later); UpdateTouchState finite-differences
    // velocity once per frame into the touch_* outputs exposed through RenderContext. Position and
    // pressure pass through directly (responsive); any smoothing is the caller's job. Callers are
    // expected on the app/render thread (like the SDL mouse handler), so plain members suffice.
    bool m_touchActive{false};      //!< True between Touch and TouchDestroy (touch_on).
    float m_touchX{0.5f};           //!< Latest touch X, [0,1] left to right (touch_x).
    float m_touchY{0.5f};           //!< Latest touch Y, [0,1] bottom to top (touch_y).
    float m_touchPressure{0.0f};    //!< Latest touch pressure, [0,1] (touch_pressure).
    float m_touchVx{0.0f};          //!< Smoothed touch X velocity, /sec (touch_vx).
    float m_touchVy{0.0f};          //!< Smoothed touch Y velocity, /sec (touch_vy).
    float m_touchPrevX{0.5f};       //!< Previous-frame X for velocity finite-differencing.
    float m_touchPrevY{0.5f};       //!< Previous-frame Y for velocity finite-differencing.
    bool m_touchWasActive{false};   //!< Active state last frame (suppresses spawn/release velocity spikes).

    // Body pose. The app writes the measured skeleton (capture thread) via SetPose; UpdatePoseState
    // smooths it once per frame (render thread) into the pose outputs exposed through RenderContext.
    // Same benign-race pattern as the seg centroid. See POSE_API.md.
    std::atomic<uint32_t> m_poseSeq{0};    //!< Bumped on each app update; render thread sees freshness.
    uint32_t m_poseSeqSeen{0};             //!< Last sequence the render thread processed.
    float m_poseMeasured[Renderer::PoseJointCount][4]{}; //!< Latest submitted x,y,z,conf (mirror-corrected).
    float m_poseSecondsSinceUpdate{1.0e3f};              //!< Time since the last app update (staleness).
    Renderer::PoseState m_pose{};                        //!< Smoothed skeleton + derived scalars.
};

} // namespace libprojectM
