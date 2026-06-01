#include "Renderer/VideoTexture.hpp"

#include "Renderer/OpenGL.h"
#include "Renderer/Shader.hpp"
#include "Renderer/VideoPreprocessShaders.hpp"

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <string>

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
    CreateTexture();
    CreateGpuResources();
}

VideoTexture::~VideoTexture()
{
    glDeleteTextures(1, &m_inputTex);
    glDeleteTextures(2, m_prevTex);
    glDeleteTextures(2, m_bgTex);
    glDeleteTextures(2, m_morphTex);
    if (m_fbo) { glDeleteFramebuffers(1, &m_fbo); }
    if (m_vao) { glDeleteVertexArrays(1, &m_vao); }
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

void VideoTexture::UpdateGPU(const AlphaParams& params)
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

    // Upload the freshly downscaled frame to the input texture.
    glBindTexture(GL_TEXTURE_2D, m_inputTex);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, m_texWidth, m_texHeight,
                    GL_RGBA, GL_UNSIGNED_BYTE, m_workBuffer.data());
    glBindTexture(GL_TEXTURE_2D, 0);

    m_writeIndex = (m_writeIndex + 1) % m_depth;

    const int readIdx = 1 - m_pingPong; // previous frame's processed/background
    const int writeIdx = m_pingPong;    // this frame's output

    // Save and override GL state we touch, so the main render pass is unaffected.
    GLint prevViewport[4]{};
    glGetIntegerv(GL_VIEWPORT, prevViewport);

    // --- Preprocessing pass: compute the mask into prev[writeIdx] / bg[writeIdx]. ---
    glBindFramebuffer(GL_FRAMEBUFFER, m_fbo);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, m_prevTex[writeIdx], 0);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, m_bgTex[writeIdx], 0);
    const GLenum drawBuffers[2] = {GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1};
    glDrawBuffers(2, drawBuffers);

    glDisable(GL_BLEND);
    glDisable(GL_DEPTH_TEST);
    glViewport(0, 0, m_texWidth, m_texHeight);

    m_preprocessShader->Bind();
    m_preprocessShader->SetUniformInt("u_mode", static_cast<int>(params.mode));
    m_preprocessShader->SetUniformFloat("u_value", params.value);
    m_preprocessShader->SetUniformFloat("u_init", params.init);
    m_preprocessShader->SetUniformFloat("u_decay", params.decay);
    m_preprocessShader->SetUniformFloat3("u_key", {m_keyR, m_keyG, m_keyB});
    m_preprocessShader->SetUniformInt("u_hasPrev", m_hasPreviousFrame ? 1 : 0);
    m_preprocessShader->SetUniformInt("u_hasBackground", m_hasBackground ? 1 : 0);
    m_preprocessShader->SetUniformInt("u_mirror", m_mirror ? 1 : 0);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, m_inputTex);
    m_preprocessShader->SetUniformInt("u_input", 0);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, m_prevTex[readIdx]);
    m_preprocessShader->SetUniformInt("u_prev", 1);
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, m_bgTex[readIdx]);
    m_preprocessShader->SetUniformInt("u_bg", 2);

    glBindVertexArray(m_vao);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);

    // --- Optional morphological cleanup of the mask (foreground-biased). ---
    // Dilation-biased close: dilate (grow + fill pinholes) then one fewer erode, so the
    // net effect grows/keeps foreground and fills holes but NEVER erodes-first (an
    // opening would shrink thin/dark foreground away). We prioritize not losing the
    // subject over removing background speckle. Operates only on the alpha channel,
    // ping-ponging through the scratch textures, ending back in prev[writeIdx].
    if (params.cleanup > 0)
    {
        int iterations = params.cleanup > 4 ? 4 : params.cleanup;
        std::vector<float> ops; // -1 = erode, +1 = dilate
        for (int i = 0; i < iterations + 1; ++i) { ops.push_back(1.0f); } // dilate (grow + fill holes)
        for (int i = 0; i < iterations; ++i) { ops.push_back(-1.0f); }    // erode (one fewer = net grow)

        m_morphShader->Bind();
        m_morphShader->SetUniformFloat2("u_texel", {1.0f / static_cast<float>(m_texWidth),
                                                    1.0f / static_cast<float>(m_texHeight)});
        const GLenum singleBuffer[1] = {GL_COLOR_ATTACHMENT0};
        uint32_t src = m_prevTex[writeIdx];
        for (size_t p = 0; p < ops.size(); ++p)
        {
            const uint32_t dst = (p + 1 == ops.size()) ? m_prevTex[writeIdx] : m_morphTex[p % 2];
            glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, dst, 0);
            glDrawBuffers(1, singleBuffer);
            m_morphShader->SetUniformFloat("u_op", ops[p]);
            glActiveTexture(GL_TEXTURE0);
            glBindTexture(GL_TEXTURE_2D, src);
            m_morphShader->SetUniformInt("u_mask", 0);
            glBindVertexArray(m_vao);
            glDrawArrays(GL_TRIANGLES, 0, 3);
            glBindVertexArray(0);
            src = dst;
        }
        // Re-attach prev[writeIdx] as color 0 for the ring copy (it already is after the
        // final pass, but make the draw-buffer state explicit).
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, m_prevTex[writeIdx], 0);
    }

    // Copy the processed frame (color attachment 0) into the ring-buffer slice.
    glReadBuffer(GL_COLOR_ATTACHMENT0);
    glBindTexture(GL_TEXTURE_3D, m_texture->TextureID());
    glCopyTexSubImage3D(GL_TEXTURE_3D, 0, 0, 0, m_writeIndex, 0, 0, m_texWidth, m_texHeight);
    glBindTexture(GL_TEXTURE_3D, 0);

    // Restore state.
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, 0);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, 0);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, 0);
    Shader::Unbind();
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
    glViewport(prevViewport[0], prevViewport[1], prevViewport[2], prevViewport[3]);

    m_pingPong = 1 - m_pingPong;
    m_hasPreviousFrame = true;
    if (params.mode == AlphaMode::BackgroundSubtract)
    {
        m_hasBackground = true;
    }
    ++m_frameCount;
}

void VideoTexture::SetChromaKey(float r, float g, float b)
{
    m_keyR = r;
    m_keyG = g;
    m_keyB = b;
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
                        case PixelFormat::RGBX:
                            sumR += p[0]; sumG += p[1]; sumB += p[2]; sumA += 255;
                            break;
                        case PixelFormat::BGRX:
                            sumR += p[2]; sumG += p[1]; sumB += p[0]; sumA += 255;
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


void VideoTexture::CreateGpuResources()
{
    // Prepend the GLSL version header the same way MilkdropStaticShaders does.
#ifdef USE_GLES
    const std::string header = "#version 300 es\n";
#else
    const std::string header = "#version 330\n";
#endif
    m_preprocessShader = std::make_unique<Shader>();
    m_preprocessShader->CompileProgram(header + kVideoPreprocessVertexShader,
                                       header + kVideoPreprocessFragmentShader);
    m_morphShader = std::make_unique<Shader>();
    m_morphShader->CompileProgram(header + kVideoPreprocessVertexShader,
                                  header + kVideoMorphFragmentShader);

    // Input texture: the uploaded downscaled camera frame (RGBA8).
    glGenTextures(1, &m_inputTex);
    glBindTexture(GL_TEXTURE_2D, m_inputTex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, m_texWidth, m_texHeight, 0,
                 GL_RGBA, GL_UNSIGNED_BYTE, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    // Ping-pong processed + background textures, float for accumulation precision.
    const std::vector<float> zeros(static_cast<size_t>(m_texWidth) * static_cast<size_t>(m_texHeight) * 4, 0.0f);
    uint32_t* pingPongTextures[6] = {&m_prevTex[0], &m_prevTex[1], &m_bgTex[0], &m_bgTex[1],
                                     &m_morphTex[0], &m_morphTex[1]};
    for (uint32_t* tex : pingPongTextures)
    {
        glGenTextures(1, tex);
        glBindTexture(GL_TEXTURE_2D, *tex);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16F, m_texWidth, m_texHeight, 0,
                     GL_RGBA, GL_FLOAT, zeros.data());
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    }
    glBindTexture(GL_TEXTURE_2D, 0);

    glGenFramebuffers(1, &m_fbo);
    glGenVertexArrays(1, &m_vao);
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