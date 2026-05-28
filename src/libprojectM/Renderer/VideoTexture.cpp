#include "Renderer/VideoTexture.hpp"

#include "Renderer/OpenGL.h"

#include <algorithm>
#include <cmath>
#include <cstdlib>

namespace libprojectM {
namespace Renderer {

VideoTexture::VideoTexture(int texWidth, int texHeight, int depth)
    : m_texWidth(texWidth)
    , m_texHeight(texHeight)
    , m_depth(depth)
    , m_sliceBytes(static_cast<size_t>(texWidth) * static_cast<size_t>(texHeight) * 4)
{
    m_stagingBuffer.resize(m_sliceBytes);
    m_workBuffer.resize(m_sliceBytes);
    m_previousRGB.resize(static_cast<size_t>(texWidth) * static_cast<size_t>(texHeight) * 3);
    CreateTexture();
}

void VideoTexture::CreateTexture()
{
    m_texture = std::make_shared<Texture>("video", GL_TEXTURE_3D,
                                          m_texWidth, m_texHeight, m_depth,
                                          GL_RGBA8, GL_RGBA, GL_UNSIGNED_BYTE, false);

    // Zero-initialize the entire 3D volume so unwritten slices sample as transparent black.
    std::vector<uint8_t> zeros(m_sliceBytes * static_cast<size_t>(m_depth), 0);
    m_texture->Update(zeros.data());
}

auto VideoTexture::GetTexture() const -> const std::shared_ptr<Texture>&
{
    return m_texture;
}

void VideoTexture::SubmitFrame(const void* data, int srcWidth, int srcHeight, PixelFormat format)
{
    if (data == nullptr || srcWidth <= 0 || srcHeight <= 0)
    {
        return;
    }

    std::lock_guard<std::mutex> lock(m_mutex);
    ConvertAndDownscale(static_cast<const uint8_t*>(data), srcWidth, srcHeight, format,
                        m_stagingBuffer.data());
    m_hasPendingFrame = true;
}

void VideoTexture::UpdateGPU(AlphaMode alphaMode, float alphaValue, float alphaInit)
{
    {
        std::lock_guard<std::mutex> lock(m_mutex);
        if (!m_hasPendingFrame)
        {
            return;
        }
        std::swap(m_workBuffer, m_stagingBuffer);
        m_hasPendingFrame = false;
    }

    ComputeAlpha(m_workBuffer.data(), alphaMode, alphaValue, alphaInit);

    m_writeIndex = (m_writeIndex + 1) % m_depth;

    glBindTexture(GL_TEXTURE_3D, m_texture->TextureID());
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glTexSubImage3D(GL_TEXTURE_3D, 0,
                    0, 0, m_writeIndex,
                    m_texWidth, m_texHeight, 1,
                    GL_RGBA, GL_UNSIGNED_BYTE,
                    m_workBuffer.data());
    glBindTexture(GL_TEXTURE_3D, 0);

    // Cache RGB for next motion-detection compare.
    const uint8_t* src = m_workBuffer.data();
    uint8_t* prev = m_previousRGB.data();
    const int pixels = m_texWidth * m_texHeight;
    for (int i = 0; i < pixels; ++i)
    {
        prev[i * 3 + 0] = src[i * 4 + 0];
        prev[i * 3 + 1] = src[i * 4 + 1];
        prev[i * 3 + 2] = src[i * 4 + 2];
    }
    m_hasPreviousFrame = true;
    ++m_frameCount;
}

void VideoTexture::ConvertAndDownscale(const uint8_t* src, int srcW, int srcH,
                                       PixelFormat fmt, uint8_t* dst)
{
    const int srcChannels = (fmt == PixelFormat::RGB) ? 3 : 4;
    const int dstW = m_texWidth;
    const int dstH = m_texHeight;

    for (int dy = 0; dy < dstH; ++dy)
    {
        int sy0 = (dy * srcH) / dstH;
        int sy1 = ((dy + 1) * srcH) / dstH;
        if (sy1 <= sy0) sy1 = sy0 + 1;
        if (sy1 > srcH) sy1 = srcH;

        for (int dx = 0; dx < dstW; ++dx)
        {
            int sx0 = (dx * srcW) / dstW;
            int sx1 = ((dx + 1) * srcW) / dstW;
            if (sx1 <= sx0) sx1 = sx0 + 1;
            if (sx1 > srcW) sx1 = srcW;

            uint32_t sumR = 0, sumG = 0, sumB = 0, sumA = 0;
            uint32_t count = 0;
            for (int sy = sy0; sy < sy1; ++sy)
            {
                const uint8_t* row = src + static_cast<size_t>(sy) * srcW * srcChannels;
                for (int sx = sx0; sx < sx1; ++sx)
                {
                    const uint8_t* p = row + sx * srcChannels;
                    switch (fmt)
                    {
                        case PixelFormat::RGB:
                            sumR += p[0]; sumG += p[1]; sumB += p[2]; sumA += 255;
                            break;
                        case PixelFormat::RGBA:
                            sumR += p[0]; sumG += p[1]; sumB += p[2]; sumA += p[3];
                            break;
                        case PixelFormat::BGRA:
                            sumR += p[2]; sumG += p[1]; sumB += p[0]; sumA += p[3];
                            break;
                    }
                    ++count;
                }
            }

            uint8_t* d = dst + (static_cast<size_t>(dy) * dstW + dx) * 4;
            if (count > 0)
            {
                d[0] = static_cast<uint8_t>(sumR / count);
                d[1] = static_cast<uint8_t>(sumG / count);
                d[2] = static_cast<uint8_t>(sumB / count);
                d[3] = static_cast<uint8_t>(sumA / count);
            }
        }
    }
}

void VideoTexture::ComputeAlpha(uint8_t* rgba, AlphaMode mode, float value, float initValue)
{
    const int pixels = m_texWidth * m_texHeight;

    if (!m_hasPreviousFrame)
    {
        // First frame: alphaInit overrides whatever mode is selected (no history yet).
        float clamped = initValue;
        if (clamped < 0.0f) clamped = 0.0f;
        if (clamped > 1.0f) clamped = 1.0f;
        const uint8_t a = static_cast<uint8_t>(clamped * 255.0f + 0.5f);
        for (int i = 0; i < pixels; ++i)
        {
            rgba[i * 4 + 3] = a;
        }
        return;
    }

    switch (mode)
    {
        case AlphaMode::Source:
            // Already set during ConvertAndDownscale.
            break;

        case AlphaMode::Motion:
        {
            const uint8_t* prev = m_previousRGB.data();
            for (int i = 0; i < pixels; ++i)
            {
                int dr = std::abs(static_cast<int>(rgba[i * 4 + 0]) - static_cast<int>(prev[i * 3 + 0]));
                int dg = std::abs(static_cast<int>(rgba[i * 4 + 1]) - static_cast<int>(prev[i * 3 + 1]));
                int db = std::abs(static_cast<int>(rgba[i * 4 + 2]) - static_cast<int>(prev[i * 3 + 2]));
                int diff = std::max({dr, dg, db});
                float a = (static_cast<float>(diff) / 255.0f) * value;
                if (a < 0.0f) a = 0.0f;
                if (a > 1.0f) a = 1.0f;
                rgba[i * 4 + 3] = static_cast<uint8_t>(a * 255.0f + 0.5f);
            }
            break;
        }

        case AlphaMode::Constant:
        {
            float clamped = value;
            if (clamped < 0.0f) clamped = 0.0f;
            if (clamped > 1.0f) clamped = 1.0f;
            const uint8_t a = static_cast<uint8_t>(clamped * 255.0f + 0.5f);
            for (int i = 0; i < pixels; ++i)
            {
                rgba[i * 4 + 3] = a;
            }
            break;
        }
    }
}

float VideoTexture::NormalizedWritePosition() const
{
    if (m_writeIndex < 0)
    {
        return 0.0f;
    }
    return (static_cast<float>(m_writeIndex) + 0.5f) / static_cast<float>(m_depth);
}

float VideoTexture::NormalizedRange() const
{
    if (m_frameCount <= 1)
    {
        return 0.0f;
    }
    const uint32_t filled = std::min<uint32_t>(m_frameCount - 1,
                                               static_cast<uint32_t>(m_depth - 1));
    return static_cast<float>(filled) / static_cast<float>(m_depth);
}

} // namespace Renderer
} // namespace libprojectM