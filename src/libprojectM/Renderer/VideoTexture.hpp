/**
* @file VideoTexture.hpp
* @brief 3D ring-buffer texture holding recent video frames for preset sampling.
*/
#pragma once

#include "Renderer/Texture.hpp"

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
    };

    enum class AlphaMode
    {
        Source = 0,             //!< Use source alpha as-is (1.0 for RGB sources, app-supplied mask for RGBA)
        Motion = 1,             //!< Alpha = magnitude of RGB difference vs. previous frame, scaled by value
        Constant = 2,           //!< Alpha = value
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
        int cleanup{0};     //!< Morphological mask cleanup iterations (0 = off). Each iter = an open + close pass.
    };

    VideoTexture(int texWidth, int texHeight, int depth);
    ~VideoTexture();

    VideoTexture(const VideoTexture&) = delete;
    auto operator=(const VideoTexture&) -> VideoTexture& = delete;

    /** @brief The underlying 3D texture; register this with TextureManager. */
    auto GetTexture() const -> const std::shared_ptr<class Texture>&;

    /**
     * @brief Submits a new video frame. Safe to call from any thread.
     * Downscales the source to the configured texture size and stages it for upload.
     * If a previous frame is still pending, it is replaced.
     */
    void SubmitFrame(const void* data, int srcWidth, int srcHeight, PixelFormat format);

    /**
     * @brief Uploads the most recently staged frame, if any, to the next ring-buffer slice.
     * Must be called on the GL thread.
     */
    void UpdateGPU(const AlphaParams& params);

    /**
     * @brief Sets the ChromaKey background color (normalized 0..1), supplied by the
     * application (it depends on the camera/scene, not the preset). Defaults to black,
     * which doubles as the virtual-green-screen sentinel.
     */
    void SetChromaKey(float r, float g, float b);

    int Width() const { return m_texWidth; }
    int Height() const { return m_texHeight; }
    int Depth() const { return m_depth; }

    /** @brief Number of frames uploaded so far. */
    uint32_t FrameCount() const { return m_frameCount; }

    /** @brief Normalized Z (slice center) of the most recently uploaded frame, in [0,1]. */
    float NormalizedWritePosition() const;

    /** @brief Normalized Z range covered by valid (filled) slices, in [0,1]. */
    float NormalizedRange() const;

private:
    void CreateTexture();
    void CreateGpuResources();
    void ConvertAndDownscale(const uint8_t* src, int srcW, int srcH, PixelFormat fmt, uint8_t* dst);

    const int m_texWidth;
    const int m_texHeight;
    const int m_depth;
    const size_t m_sliceBytes;

    std::shared_ptr<class Texture> m_texture;

    std::mutex m_mutex;
    std::vector<uint8_t> m_stagingBuffer;
    bool m_hasPendingFrame{false};

    std::vector<uint8_t> m_workBuffer; //!< Downscaled RGBA frame ready for GPU upload.
    bool m_hasPreviousFrame{false};
    bool m_hasBackground{false};       //!< Whether the GPU background model has been seeded.

    float m_keyR{0.0f}; //!< ChromaKey background color (app-supplied, normalized). Default black sentinel.
    float m_keyG{0.0f};
    float m_keyB{0.0f};
    int m_writeIndex{-1};
    uint32_t m_frameCount{0};

    // GPU preprocessing resources (raw GL object names; created/destroyed on the GL thread).
    std::unique_ptr<Shader> m_preprocessShader;
    std::unique_ptr<Shader> m_morphShader; //!< Erode/dilate pass for mask cleanup.
    uint32_t m_inputTex{0};      //!< 2D RGBA8: the uploaded downscaled camera frame.
    uint32_t m_prevTex[2]{0, 0}; //!< 2D RGBA16F ping-pong: processed [rawRGB, alpha].
    uint32_t m_bgTex[2]{0, 0};   //!< 2D RGBA16F ping-pong: background model.
    uint32_t m_morphTex[2]{0, 0};//!< 2D RGBA16F scratch ping-pong for morphology passes.
    uint32_t m_fbo{0};
    uint32_t m_vao{0};
    int m_pingPong{0};           //!< Index of the ping-pong slot written this frame.
};

} // namespace Renderer
} // namespace libprojectM