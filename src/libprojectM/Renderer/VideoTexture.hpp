/**
* @file VideoTexture.hpp
* @brief 3D ring-buffer texture holding recent video frames for preset sampling.
*/
#pragma once

#include "Renderer/Texture.hpp"

#include <chrono>
#include <cstdint>
#include <memory>
#include <mutex>
#include <vector>

namespace libprojectM {
namespace Renderer {

class Shader;

/**
 * @brief Maintains a GL_TEXTURE_3D where X/Y are spatial pixels and Z is recent time.
 *
 * The application submits raw video frames from any thread via SubmitFrame(). On the
 * GL thread, UpdateGPU() uploads the most recent staged frame to the next slice of
 * the ring buffer, optionally computing a per-pixel alpha channel under preset control.
 */
class VideoTexture
{
public:
    enum class PixelFormat
    {
        RGB = 0,  //!< 3 bytes per pixel, alpha defaults to 1.0
        RGBA = 1, //!< 4 bytes per pixel
        BGRA = 2, //!< 4 bytes per pixel, channels swapped
        RGBX = 3, //!< 4 bytes per pixel, alpha byte ignored and forced to 1.0 (opaque source, e.g. a camera)
        BGRX = 4, //!< 4 bytes per pixel, channels swapped, alpha byte ignored and forced to 1.0
    };

    enum class AlphaMode
    {
        Source = 0,             //!< Use source alpha as-is (1.0 for RGB sources, app-supplied mask for RGBA)
        Constant = 1,           //!< Alpha = value
        Motion = 2,             //!< Alpha = magnitude of RGB difference vs. previous frame, scaled by value
        MotionDecay = 3,        //!< Alpha = max(motion, previous_alpha * decay) — motion lingers and fades
        ChromaKey = 4,          //!< Alpha = foreground-ness; background = pixels near keyColor. value=tolerance (0=exact)
        BackgroundSubtract = 5, //!< Alpha = foreground vs. a temporally-averaged background. value=threshold, decay=learn rate
    };

    /**
     * @brief Parameters controlling how the per-pixel alpha (mask) is derived.
     * The meaning of value/decay depends on the mode (see AlphaMode).
     */
    struct AlphaParams
    {
        AlphaMode mode{AlphaMode::Source};
        float value{1.0f};  //!< Motion/Decay: scale. Constant: alpha. ChromaKey: tolerance (0..1, 0=exact). BgSubtract: threshold (0..1).
        float init{1.0f};   //!< Alpha for the very first frame (no history yet).
        float decay{0.9f};  //!< MotionDecay: per-frame persistence (0..1). BackgroundSubtract: background learning rate (set low, e.g. 0.02).
        int cleanup{0};     //!< Morphological mask cleanup iterations (0 = off). Each iter = an open + close pass. Ignored when refine is set.
        bool refine{false}; //!< Run the shared refinement back-end (guided fill -> matte -> temporal -> feather) on the prior's mask.
    };

    VideoTexture(int texWidth, int texHeight, int depth);
    ~VideoTexture();

    VideoTexture(const VideoTexture&) = delete;
    auto operator=(const VideoTexture&) -> VideoTexture& = delete;

    /** @brief The underlying 3D texture; register this with TextureManager. */
    auto GetTexture() const -> const std::shared_ptr<class Texture>&;

    /**
     * @brief 2D RGBA16F mask/analysis buffer derived during preprocessing; register as "mask".
     *
     * Current-frame, video-space, sampled like the input. Channels:
     *   R = seg (foreground/human matte = the app-supplied alpha), G = seg blur,
     *   B = motion (frame-diff vs. previous frame), A = motion decay.
     * See MASK_LAYERS.md.
     */
    auto GetMaskTexture() const -> const std::shared_ptr<class Texture>&;

    /**
     * @brief Submits a new video frame. Safe to call from any thread.
     * Downscales the source to the configured texture size and stages it for upload.
     * If a previous frame is still pending, it is replaced.
     */
    void SubmitFrame(const void* data, int srcWidth, int srcHeight, PixelFormat format);

    /**
     * @brief Submits a coarse alpha weight map ("gate") multiplied into the app-supplied matte.
     *
     * The application may derive a foreground matte and, separately, a low-resolution weight map
     * saying how much of that matte to keep (e.g. a depth or pose gate that fades out background
     * people). Applying it here rather than on the CPU keeps the full-resolution multiply off the
     * capture thread; the grid is uploaded as a small texture and sampled bilinearly, which
     * feathers it exactly as a CPU bilinear apply would.
     *
     * Weights are in [0,1] (1 = keep), row-major, in the same un-mirrored space as the submitted
     * frame. The map persists until replaced, so it must be resubmitted whenever it changes.
     * Safe to call from any thread. Applies to SubmitFrame; GPU-submitted frames (SubmitFrameGPU)
     * already carry a finished mask and are not gated.
     */
    void SubmitAlphaGate(const float* weights, int gridWidth, int gridHeight);

    /**
     * @brief GL texture name of the RGBA8 input surface, at the configured texture size.
     *
     * For applications that preprocess frames on the GPU (e.g. a depth camera that
     * composites a real foreground mask into alpha). Render your finished RGBA frame
     * into this texture, then call SubmitFrameGPU(). GL thread only.
     */
    uint32_t InputTextureId() const { return m_inputTex; }

    /**
     * @brief Marks the input surface (see InputTextureId) as filled by the application.
     *
     * The next UpdateGPU() copies it into the ring buffer verbatim: RGB as drawn and
     * alpha taken as the app-supplied mask. The preset's alpha mode and mask cleanup are
     * bypassed for this frame (the application owns the mask). GL thread only.
     */
    void SubmitFrameGPU();

    /**
     * @brief Uploads the most recently staged frame, if any, to the next ring-buffer slice.
     * Must be called on the GL thread.
     * @param params Fixed alpha-mode parameters (used when alphaShader is null).
     * @param alphaShader Optional preset "combine" shader that authors the alpha (and optionally
     *        rgb) written into the history from the live frame, the mask buffer and the history.
     *        When set, the fixed alpha-mode refinement/cleanup is bypassed for this frame.
     */
    void UpdateGPU(const AlphaParams& params, class Shader* alphaShader = nullptr);

    /**
     * @brief Sets the ChromaKey background color (normalized 0..1), supplied by the
     * application (it depends on the camera/scene, not the preset). Defaults to black,
     * which doubles as the virtual-green-screen sentinel.
     */
    void SetChromaKey(float r, float g, float b);

    /**
     * @brief Enables or disables horizontal mirroring of the incoming camera frames.
     * Applied during GPU preprocessing, so it affects every alpha mode. Off by default.
     */
    void SetMirror(bool mirror) { m_mirror = mirror; }

    /**
     * @brief Application-global mask-mode override. Foreground extraction is usually a
     * scene/hardware property the app owns, not the preset. When mode >= 0 it overrides the
     * preset's per-frame alpha mode and enables/disables the refinement back-end; mode < 0
     * (the default) leaves masking under preset control.
     * @param mode AlphaMode value, or -1 to defer to the preset.
     * @param refine Run the shared refinement back-end when this override is active.
     */
    void SetMaskMode(int mode, bool refine) { m_appMaskMode = mode; m_appRefine = refine; }

    int Width() const { return m_texWidth; }
    int Height() const { return m_texHeight; }
    int Depth() const { return m_depth; }

    /** @brief Number of frames uploaded so far. */
    uint32_t FrameCount() const { return m_frameCount; }

    /** @brief Normalized Z (slice center) of the most recently uploaded frame, in [0,1]. */
    float NormalizedWritePosition() const;

    /** @brief Normalized Z range covered by valid (filled) slices, in [0,1]. */
    float NormalizedRange() const;

    /**
     * @brief Wall-clock duration spanned by the valid slices, in seconds (0 until measured).
     *
     * The ring advances once per submitted source frame, whose rate may differ from the render
     * rate, so this is derived from a running average of the measured time between advances. Lets
     * presets sample by elapsed time: age = seconds_ago / BufferSeconds().
     */
    float BufferSeconds() const;

private:
    void CreateTexture();
    void CreateGpuResources();
    void ConvertAndDownscale(const uint8_t* src, int srcW, int srcH, PixelFormat fmt, uint8_t* dst);

    /**
     * @brief Derives the mask buffer (seg/seg-blur/motion/motion-decay) into m_maskTexture.
     * Assumes m_fbo is bound, the viewport is the texture size, samplers are cleared and m_vao is
     * available. Leaves no mask FBO attachment behind. Used both before the combine pass (so a
     * video_ shader can read MaskSeg/MaskMotion of the current frame) and, in the fixed path, after
     * the slice copy.
     * @param readIdx Ping-pong index of the previous frame's processed texture (motion reference).
     */
    void RunMaskBuffer(int readIdx);

    const int m_texWidth;
    const int m_texHeight;
    const int m_depth;
    const size_t m_sliceBytes;

    std::shared_ptr<class Texture> m_texture;
    std::shared_ptr<class Texture> m_maskTexture; //!< 2D RGBA16F mask buffer (seg/seg-blur/motion/motion-decay).

    std::mutex m_mutex;
    std::vector<uint8_t> m_stagingBuffer;
    bool m_hasPendingFrame{false};
    bool m_pendingFrameIsGpu{false}; //!< Pending frame was rendered into m_inputTex by the app (see SubmitFrameGPU).

    std::vector<uint8_t> m_workBuffer; //!< Downscaled RGBA frame ready for GPU upload.
    bool m_hasPreviousFrame{false};
    bool m_hasBackground{false};       //!< Whether the GPU background model has been seeded.

    float m_keyR{0.0f}; //!< ChromaKey background color (app-supplied, normalized). Default black sentinel.
    float m_keyG{0.0f};
    float m_keyB{0.0f};
    bool m_mirror{false}; //!< Horizontally mirror incoming camera frames during preprocessing.
    int m_appMaskMode{-1}; //!< App-global alpha-mode override (>=0 wins over the preset; -1 = preset-controlled).
    bool m_appRefine{false}; //!< Run the refinement back-end when the app override is active.
    int m_writeIndex{-1};
    uint32_t m_frameCount{0};

    // Cadence of actual slice advances, for BufferSeconds() (source rate may differ from render rate).
    std::chrono::steady_clock::time_point m_lastAdvanceTime{};
    bool m_hasLastAdvance{false};
    double m_secondsPerSlice{0.0}; //!< EMA of wall-clock seconds between slice advances.

    // GPU preprocessing resources (raw GL object names; created/destroyed on the GL thread).
    std::unique_ptr<Shader> m_preprocessShader;
    std::unique_ptr<Shader> m_morphShader; //!< Erode/dilate pass for mask cleanup.
    std::unique_ptr<Shader> m_fillShader;     //!< B1: color-guided joint-bilateral up-fill.
    std::unique_ptr<Shader> m_matteShader;    //!< B2: trimap + edge-snapping matte refine.
    std::unique_ptr<Shader> m_temporalShader; //!< B3: temporal EMA stabilization.
    std::unique_ptr<Shader> m_featherShader;  //!< B4: composite + feather.
    std::unique_ptr<Shader> m_maskGatherShader; //!< Gathers seg/motion into the mask buffer.
    std::unique_ptr<Shader> m_maskBlurShader;   //!< Blurs the seg channel into the mask buffer's G.
    std::vector<uint8_t> m_gateStaging; //!< Pending gate weights, quantized to 8 bits (see SubmitAlphaGate).
    std::vector<uint8_t> m_gateWork;    //!< Gate weights being uploaded (swapped out of staging).
    int m_gateStagingW{0};
    int m_gateStagingH{0};
    bool m_gatePending{false};   //!< A new gate map is staged for upload.
    bool m_hasGate{false};       //!< A gate map has been uploaded and is live.
    int m_gateW{0};              //!< Dimensions currently allocated in m_gateTex.
    int m_gateH{0};

    uint32_t m_gateTex{0};       //!< 2D R8: coarse alpha weight map (1 = keep), bilinearly sampled.
    uint32_t m_inputTex{0};      //!< 2D RGBA8: the uploaded downscaled camera frame.
    uint32_t m_prevTex[2]{0, 0}; //!< 2D RGBA16F ping-pong: processed [rawRGB, alpha].
    uint32_t m_bgTex[2]{0, 0};   //!< 2D RGBA16F ping-pong: background model.
    uint32_t m_morphTex[2]{0, 0};//!< 2D RGBA16F scratch ping-pong for morphology / refinement passes.
    uint32_t m_stableTex[2]{0, 0};//!< 2D RGBA16F ping-pong: temporal-feedback (stabilized) alpha.
    uint32_t m_fbo{0};
    uint32_t m_vao{0};
    uint32_t m_vbo{0}; //!< Fullscreen-triangle vertex buffer (Apple GL rejects attributeless draws).
    int m_pingPong{0};           //!< Index of the ping-pong slot written this frame.
    int m_pingStable{0};         //!< Index of the temporal-feedback slot written this frame.
    bool m_hasStable{false};     //!< Whether a stabilized previous frame exists for temporal blending.
};

} // namespace Renderer
} // namespace libprojectM