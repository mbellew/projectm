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
        Source = 0,   //!< Use source alpha as-is (1.0 for RGB sources)
        Motion = 1,   //!< Alpha = magnitude of RGB difference vs. previous frame, scaled by value
        Constant = 2, //!< Alpha = value
    };

    VideoTexture(int texWidth, int texHeight, int depth);
    ~VideoTexture() = default;

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
    void UpdateGPU(AlphaMode alphaMode, float alphaValue, float alphaInit);

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
    void ConvertAndDownscale(const uint8_t* src, int srcW, int srcH, PixelFormat fmt, uint8_t* dst);
    void ComputeAlpha(uint8_t* rgba, AlphaMode mode, float value, float initValue);

    const int m_texWidth;
    const int m_texHeight;
    const int m_depth;
    const size_t m_sliceBytes;

    std::shared_ptr<class Texture> m_texture;

    std::mutex m_mutex;
    std::vector<uint8_t> m_stagingBuffer;
    bool m_hasPendingFrame{false};

    std::vector<uint8_t> m_workBuffer;
    std::vector<uint8_t> m_previousRGB;
    bool m_hasPreviousFrame{false};
    int m_writeIndex{-1};
    uint32_t m_frameCount{0};
};

} // namespace Renderer
} // namespace libprojectM