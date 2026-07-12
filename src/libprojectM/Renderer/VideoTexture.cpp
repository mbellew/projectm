#include "Renderer/VideoTexture.hpp"

#include "Renderer/OpenGL.h"
#include "Renderer/Shader.hpp"
#include "Renderer/VideoPreprocessShaders.hpp"

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cstring>
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
    glDeleteTextures(2, m_stableTex);
    if (m_gateTex) { glDeleteTextures(1, &m_gateTex); }
    if (m_fbo) { glDeleteFramebuffers(1, &m_fbo); }
    if (m_vao) { glDeleteVertexArrays(1, &m_vao); }
    if (m_vbo) { glDeleteBuffers(1, &m_vbo); }
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

auto VideoTexture::GetMaskTexture() const -> const std::shared_ptr<Texture>&
{
    return m_maskTexture;
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
    m_pendingFrameIsGpu = false;
}

void VideoTexture::SubmitAlphaGate(const float* weights, int gridWidth, int gridHeight)
{
    if (weights == nullptr || gridWidth <= 0 || gridHeight <= 0)
    {
        return;
    }

    // Quantize to 8 bits here, on the caller's thread: the gate is a smooth 0..1 keep weight that
    // the shader samples bilinearly, so 1/255 steps are far below what a feathered fade resolves,
    // and R8 is filterable everywhere (R32F linear filtering is not guaranteed on GLES).
    const size_t n = static_cast<size_t>(gridWidth) * static_cast<size_t>(gridHeight);
    std::lock_guard<std::mutex> lock(m_mutex);
    m_gateStaging.resize(n);
    for (size_t i = 0; i < n; ++i)
    {
        const float w = (weights[i] < 0.0f) ? 0.0f : (weights[i] > 1.0f ? 1.0f : weights[i]);
        m_gateStaging[i] = static_cast<uint8_t>(w * 255.0f + 0.5f);
    }
    m_gateStagingW = gridWidth;
    m_gateStagingH = gridHeight;
    m_gatePending = true;
}

void VideoTexture::SubmitFrameGPU()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    m_hasPendingFrame = true;
    m_pendingFrameIsGpu = true;
}

void VideoTexture::UpdateGPU(const AlphaParams& params, Shader* alphaShader)
{
    bool gpuFrame = false;
    bool gatePending = false;
    int gateW = 0;
    int gateH = 0;
    {
        std::lock_guard<std::mutex> lock(m_mutex);
        if (!m_hasPendingFrame)
        {
            return;
        }
        gpuFrame = m_pendingFrameIsGpu;
        if (!gpuFrame)
        {
            std::swap(m_workBuffer, m_stagingBuffer);
        }
        m_hasPendingFrame = false;

        gatePending = m_gatePending;
        if (gatePending)
        {
            std::swap(m_gateWork, m_gateStaging);
            gateW = m_gateStagingW;
            gateH = m_gateStagingH;
            m_gatePending = false;
        }
    }

    // Measure the wall-clock cadence of actual slice advances. Frames are submitted at the
    // source rate, which may differ from the render rate, so this EMA of seconds-per-slice is
    // what lets a preset convert a wall-clock duration (e.g. one beat) into a buffer age.
    const auto now = std::chrono::steady_clock::now();
    if (m_hasLastAdvance)
    {
        const double dt = std::chrono::duration<double>(now - m_lastAdvanceTime).count();
        if (dt > 1e-4 && dt < 10.0)
        {
            m_secondsPerSlice = (m_secondsPerSlice <= 0.0) ? dt : m_secondsPerSlice * 0.9 + dt * 0.1;
        }
    }
    m_lastAdvanceTime = now;
    m_hasLastAdvance = true;

    if (!gpuFrame)
    {
        // Upload the freshly downscaled frame to the input texture. For the GPU path the
        // application has already rendered its finished frame into m_inputTex.
        glBindTexture(GL_TEXTURE_2D, m_inputTex);
        glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
        glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, m_texWidth, m_texHeight,
                        GL_RGBA, GL_UNSIGNED_BYTE, m_workBuffer.data());
        glBindTexture(GL_TEXTURE_2D, 0);
    }

    if (gatePending)
    {
        // Reallocate only when the grid dimensions change (they follow the capture aspect, so in
        // practice once). GL_RED + R8 with linear filtering: the shader's bilinear fetch is what
        // feathers the coarse grid across the full-resolution matte.
        glBindTexture(GL_TEXTURE_2D, m_gateTex);
        glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
        if (gateW != m_gateW || gateH != m_gateH)
        {
            glTexImage2D(GL_TEXTURE_2D, 0, GL_R8, gateW, gateH, 0,
                         GL_RED, GL_UNSIGNED_BYTE, m_gateWork.data());
            m_gateW = gateW;
            m_gateH = gateH;
        }
        else
        {
            glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, gateW, gateH,
                            GL_RED, GL_UNSIGNED_BYTE, m_gateWork.data());
        }
        glBindTexture(GL_TEXTURE_2D, 0);
        m_hasGate = true;
    }

    // Resolve the masking mode and refinement for this frame:
    //  - App-provided GPU frames carry a finished RGB + mask -> pass through verbatim (Source).
    //  - An app-global override (>=0) wins over the preset (foreground extraction is a
    //    scene/hardware property the app owns).
    //  - Otherwise the preset's per-frame alpha mode applies.
    AlphaMode effectiveMode;
    bool refine;
    if (gpuFrame)
    {
        effectiveMode = AlphaMode::Source;
        refine = false;
    }
    else if (m_appMaskMode >= 0)
    {
        effectiveMode = static_cast<AlphaMode>(m_appMaskMode);
        refine = m_appRefine;
    }
    else
    {
        effectiveMode = params.mode;
        refine = params.refine;
    }
    // The refinement back-end's matte stage subsumes morphological cleanup.
    const int effectiveCleanup = (gpuFrame || refine) ? 0 : params.cleanup;

    m_writeIndex = (m_writeIndex + 1) % m_depth;

    const int readIdx = 1 - m_pingPong; // previous frame's processed/background
    const int writeIdx = m_pingPong;    // this frame's output

    // Save and override GL state we touch, so the main render pass is unaffected.
    GLint prevViewport[4]{};
    glGetIntegerv(GL_VIEWPORT, prevViewport);

    // The main render leaves GL sampler objects bound to texture units (with mipmap filtering).
    // Our preprocess textures have no mipmaps, so an inherited sampler makes them incomplete and
    // the draw fails with GL_INVALID_OPERATION (silent black output). Use the textures' own
    // parameters by clearing the sampler binding on every unit we touch.
    for (int unit = 0; unit < 4; ++unit) { glBindSampler(unit, 0); }

    // --- Prior pass: compute [rgb, alpha] into prev[writeIdx], and (BackgroundSubtract only)
    // the updated background model into bg[writeIdx]. Two SINGLE-output draws rather than one
    // MRT draw: Apple's GL core profile rejects multi-render-target draws here with
    // GL_INVALID_OPERATION, silently producing no output (black). ---
    const GLenum singleBuffer[1] = {GL_COLOR_ATTACHMENT0};
    const glm::vec2 texel{1.0f / static_cast<float>(m_texWidth), 1.0f / static_cast<float>(m_texHeight)};

    glBindFramebuffer(GL_FRAMEBUFFER, m_fbo);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, 0, 0); // ensure no stale MRT attachment
    glDrawBuffers(1, singleBuffer);

    glDisable(GL_BLEND);
    glDisable(GL_DEPTH_TEST);
    glViewport(0, 0, m_texWidth, m_texHeight);

    m_preprocessShader->Bind();
    m_preprocessShader->SetUniformInt("u_mode", static_cast<int>(effectiveMode));
    m_preprocessShader->SetUniformFloat("u_value", params.value);
    m_preprocessShader->SetUniformFloat("u_init", params.init);
    m_preprocessShader->SetUniformFloat("u_decay", params.decay);
    m_preprocessShader->SetUniformFloat3("u_key", {m_keyR, m_keyG, m_keyB});
    m_preprocessShader->SetUniformInt("u_hasPrev", m_hasPreviousFrame ? 1 : 0);
    m_preprocessShader->SetUniformInt("u_hasBackground", m_hasBackground ? 1 : 0);
    m_preprocessShader->SetUniformInt("u_mirror", m_mirror ? 1 : 0);
    // GPU-submitted frames already carry a finished mask -- the app owns it, so don't gate it.
    const int gateOn = (m_hasGate && !gpuFrame) ? 1 : 0;
    m_preprocessShader->SetUniformInt("u_hasGate", gateOn);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, m_inputTex);
    m_preprocessShader->SetUniformInt("u_input", 0);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, m_prevTex[readIdx]);
    m_preprocessShader->SetUniformInt("u_prev", 1);
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, m_bgTex[readIdx]);
    m_preprocessShader->SetUniformInt("u_bg", 2);
    glActiveTexture(GL_TEXTURE3);
    glBindTexture(GL_TEXTURE_2D, m_gateTex);
    m_preprocessShader->SetUniformInt("u_gate", 3);

    glBindVertexArray(m_vao);

    // Draw A: the processed frame + mask -> prev[writeIdx].
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, m_prevTex[writeIdx], 0);
    m_preprocessShader->SetUniformInt("u_outputSelect", 0);
    glDrawArrays(GL_TRIANGLES, 0, 3);

    // Draw B (BackgroundSubtract only): the updated background model -> bg[writeIdx].
    if (effectiveMode == AlphaMode::BackgroundSubtract)
    {
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, m_bgTex[writeIdx], 0);
        m_preprocessShader->SetUniformInt("u_outputSelect", 1);
        glDrawArrays(GL_TRIANGLES, 0, 3);
    }
    glBindVertexArray(0);

    uint32_t finalTex = m_prevTex[writeIdx]; // prior output, unless replaced by a refinement below

    if (alphaShader != nullptr)
    {
        // --- Preset-authored combine pass. The video_ shader computes the alpha (and optionally
        // rgb) written into the history. Run the mask buffer FIRST so the shader can read
        // MaskSeg/MaskMotion/... of the CURRENT frame, then combine. The fixed-mode refinement and
        // cleanup are bypassed (the preset owns the alpha). ---
        RunMaskBuffer(readIdx);

        glDrawBuffers(1, singleBuffer);
        glBindVertexArray(m_vao);
        alphaShader->Bind();

        // History z mapping for GetVideo: this frame's slice is not written yet, so the most recent
        // VALID frame is the previous slice. Point video_z_write at it; video_z_range still reflects
        // the frames filled so far (m_frameCount is incremented at the end of this call).
        const int prevIndex = (m_writeIndex - 1 + m_depth) % m_depth;
        alphaShader->SetUniformFloat("video_z_write", (static_cast<float>(prevIndex) + 0.5f) / static_cast<float>(m_depth));
        alphaShader->SetUniformFloat("video_z_range", NormalizedRange());

        // texsize (_c7) for THIS pass = the video texture's own size, not the viewport's: the
        // pass renders at video resolution, so texsize.zw is one video texel. Without it a
        // video_ shader cannot offset by a texel and so cannot run any kernel (blur, edges).
        // Presets use this to PRE-COMPUTE per-pixel work here -- at video res, once -- and
        // stash the scalar result in the history alpha, instead of recomputing it per render
        // pixel in the warp shader.
        alphaShader->SetUniformFloat4("_c7", {static_cast<float>(m_texWidth),
                                              static_cast<float>(m_texHeight),
                                              1.0f / static_cast<float>(m_texWidth),
                                              1.0f / static_cast<float>(m_texHeight)});

        // The live frame comes from prev[writeIdx]: the preprocess pass already mirrored it and
        // carried the alpha-mode result in its alpha (the video_ shader's default ret_a), so
        // GetVideoIn stays aligned with the mirrored mask buffer.
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, m_prevTex[writeIdx]);
        alphaShader->SetUniformInt("sampler_video_in", 0);
        glActiveTexture(GL_TEXTURE1);
        glBindTexture(GL_TEXTURE_2D, m_maskTexture->TextureID());
        alphaShader->SetUniformInt("sampler_fc_mask", 1);
        glActiveTexture(GL_TEXTURE2);
        glBindTexture(GL_TEXTURE_3D, m_texture->TextureID());
        alphaShader->SetUniformInt("sampler_fw_video", 2);

        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, m_morphTex[0], 0);
        glDrawArrays(GL_TRIANGLES, 0, 3);

        // Release the history from unit 2 before the slice copy rebinds it as the copy target.
        glActiveTexture(GL_TEXTURE2);
        glBindTexture(GL_TEXTURE_3D, 0);
        glBindVertexArray(0);
        finalTex = m_morphTex[0];
    }
    else if (refine)
    {
        // --- Shared refinement back-end (see VIDEO_MASKING_PIPELINE.md):
        //     B1 guided fill -> B2 matte -> B3 temporal -> B4 feather. ---
        glDrawBuffers(1, singleBuffer);
        glBindVertexArray(m_vao);

        // Helper for the single-input [rgb,alpha] passes (fill/matte/feather use "u_proc").
        auto runPass = [&](Shader& shader, uint32_t srcTex, uint32_t dstTex) {
            glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, dstTex, 0);
            glActiveTexture(GL_TEXTURE0);
            glBindTexture(GL_TEXTURE_2D, srcTex);
            shader.SetUniformInt("u_proc", 0);
            glDrawArrays(GL_TRIANGLES, 0, 3);
        };

        // B1: color-guided up-fill, prev[writeIdx] -> morph[0].
        m_fillShader->Bind();
        m_fillShader->SetUniformFloat2("u_texel", texel);
        m_fillShader->SetUniformFloat("u_sigmaColor", 0.12f);
        runPass(*m_fillShader, m_prevTex[writeIdx], m_morphTex[0]);

        // B2: trimap + matte, morph[0] -> morph[1].
        m_matteShader->Bind();
        m_matteShader->SetUniformFloat2("u_texel", texel);
        m_matteShader->SetUniformFloat("u_lo", 0.2f);
        m_matteShader->SetUniformFloat("u_hi", 0.8f);
        m_matteShader->SetUniformFloat("u_sigmaColor", 0.1f);
        runPass(*m_matteShader, m_morphTex[0], m_morphTex[1]);

        // B3: temporal EMA, (morph[1], stable[read]) -> stable[write].
        const int stWrite = 1 - m_pingStable;
        m_temporalShader->Bind();
        m_temporalShader->SetUniformInt("u_hasPrev", m_hasStable ? 1 : 0);
        // Temporal-EMA blend toward the current frame: 1.0 = no smoothing (no
        // lag), lower = steadier but the mask trails motion. 0.8 keeps light
        // flicker suppression while tracking movement tightly.
        m_temporalShader->SetUniformFloat("u_rate", 0.8f);
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, m_stableTex[stWrite], 0);
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, m_morphTex[1]);
        m_temporalShader->SetUniformInt("u_cur", 0);
        glActiveTexture(GL_TEXTURE1);
        glBindTexture(GL_TEXTURE_2D, m_stableTex[m_pingStable]);
        m_temporalShader->SetUniformInt("u_prev", 1);
        glDrawArrays(GL_TRIANGLES, 0, 3);

        // B4: composite + feather, stable[write] -> morph[0] (the ring source).
        m_featherShader->Bind();
        m_featherShader->SetUniformFloat("u_lo", 0.25f);
        m_featherShader->SetUniformFloat("u_hi", 0.75f);
        runPass(*m_featherShader, m_stableTex[stWrite], m_morphTex[0]);

        glBindVertexArray(0);
        m_pingStable = stWrite;
        m_hasStable = true;
        finalTex = m_morphTex[0];
    }
    else if (effectiveCleanup > 0)
    {
        // --- Optional morphological cleanup of the mask (foreground-biased). ---
        // Dilation-biased close: dilate (grow + fill pinholes) then one fewer erode, so the
        // net effect grows/keeps foreground and fills holes but NEVER erodes-first (an
        // opening would shrink thin/dark foreground away). We prioritize not losing the
        // subject over removing background speckle. Operates only on the alpha channel,
        // ping-ponging through the scratch textures, ending back in prev[writeIdx].
        int iterations = effectiveCleanup > 4 ? 4 : effectiveCleanup;
        std::vector<float> ops; // -1 = erode, +1 = dilate
        for (int i = 0; i < iterations + 1; ++i) { ops.push_back(1.0f); } // dilate (grow + fill holes)
        for (int i = 0; i < iterations; ++i) { ops.push_back(-1.0f); }    // erode (one fewer = net grow)

        m_morphShader->Bind();
        m_morphShader->SetUniformFloat2("u_texel", texel);
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
    }

    // Copy the final processed frame into the ring-buffer slice.
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, finalTex, 0);
    glDrawBuffers(1, singleBuffer);
    glReadBuffer(GL_COLOR_ATTACHMENT0);
    glBindTexture(GL_TEXTURE_3D, m_texture->TextureID());
    glCopyTexSubImage3D(GL_TEXTURE_3D, 0, 0, 0, m_writeIndex, 0, 0, m_texWidth, m_texHeight);
    glBindTexture(GL_TEXTURE_3D, 0);

    // Mask buffer: in the fixed path it is derived here (after the slice copy) for the NEXT frame's
    // warp/comp GetMask. With a combine shader it was already derived above (before the combine, so
    // the shader could read the current frame's MaskSeg/MaskMotion).
    if (alphaShader == nullptr)
    {
        RunMaskBuffer(readIdx);
    }

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
    if (effectiveMode == AlphaMode::BackgroundSubtract)
    {
        m_hasBackground = true;
    }
    ++m_frameCount;
}

void VideoTexture::RunMaskBuffer(int readIdx)
{
    // --- Mask buffer (MASK_LAYERS.md): derive seg/seg-blur/motion into m_maskTexture. The
    // gather pass composes (seg, 0, motion, 0) into a scratch surface (morph[1], free here),
    // then the blur pass fills G and copies the result into the registered mask texture.
    // Assumes m_fbo is bound, the viewport is the texture size and samplers are cleared. ---
    const GLenum singleBuffer[1] = {GL_COLOR_ATTACHMENT0};
    const glm::vec2 texel{1.0f / static_cast<float>(m_texWidth), 1.0f / static_cast<float>(m_texHeight)};

    glDrawBuffers(1, singleBuffer);
    glBindVertexArray(m_vao);

    m_maskGatherShader->Bind();
    m_maskGatherShader->SetUniformInt("u_hasPrev", m_hasPreviousFrame ? 1 : 0);
    m_maskGatherShader->SetUniformInt("u_mirror", m_mirror ? 1 : 0);
    m_maskGatherShader->SetUniformFloat("u_motionScale", 4.0f);
    m_maskGatherShader->SetUniformFloat("u_decay", 0.9f);
    // Gate the seg channel to match the processed alpha; seg is read from the input matte, so it
    // would otherwise still carry the people the gate removed.
    m_maskGatherShader->SetUniformInt("u_hasGate", m_hasGate ? 1 : 0);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, m_morphTex[1], 0);
    // seg comes from the INPUT matte (app-supplied alpha), so the mask buffer is independent of the
    // preset's video_alpha_mode; rgb from the same input drives the motion diff vs. the prev frame.
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, m_inputTex);
    m_maskGatherShader->SetUniformInt("u_input", 0);
    glActiveTexture(GL_TEXTURE3);
    glBindTexture(GL_TEXTURE_2D, m_gateTex);
    m_maskGatherShader->SetUniformInt("u_gate", 3);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, m_prevTex[readIdx]);
    m_maskGatherShader->SetUniformInt("u_prev", 1);
    // Motion-decay reads last frame's mask (the registered texture still holds it; the blur pass
    // below overwrites it only afterward, so this is a safe self-read, not a feedback loop).
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, m_maskTexture->TextureID());
    m_maskGatherShader->SetUniformInt("u_prevMask", 2);
    glDrawArrays(GL_TRIANGLES, 0, 3);

    // Unbind the mask from unit 2 before it becomes the render target, so it's never both an
    // FBO attachment and a bound texture (avoids the Apple GL feedback-loop warning).
    glBindTexture(GL_TEXTURE_2D, 0);

    m_maskBlurShader->Bind();
    m_maskBlurShader->SetUniformFloat2("u_texel", texel);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, m_maskTexture->TextureID(), 0);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, m_morphTex[1]);
    m_maskBlurShader->SetUniformInt("u_mask", 0);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glBindVertexArray(0);

    // Detach the mask texture so it isn't left as a live FBO attachment.
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, 0, 0);
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

    // Fast path: the source already matches the texture, so there is nothing to filter -- this is
    // a plain format conversion. It is a common case, not a corner one: the texture's long side
    // follows the display aspect, so a 4:3 display yields a 640x480 texture and the camera
    // typically negotiates exactly 640x480. The general loop below would still charge full box
    // filter price for it -- four integer divides per destination pixel, a format switch *inside*
    // the sample loop, and a final divide by a count that is always 1.
    if (srcW == dstW && srcH == dstH)
    {
        const size_t n = static_cast<size_t>(dstW) * static_cast<size_t>(dstH);
        switch (fmt)
        {
            case PixelFormat::RGB:
                for (size_t i = 0; i < n; ++i)
                {
                    const uint8_t* p = src + i * 3;
                    uint8_t* d = dst + i * 4;
                    d[0] = p[0]; d[1] = p[1]; d[2] = p[2]; d[3] = 255;
                }
                break;
            case PixelFormat::RGBA:
                std::memcpy(dst, src, n * 4);
                break;
            case PixelFormat::RGBX:
                for (size_t i = 0; i < n; ++i)
                {
                    const uint8_t* p = src + i * 4;
                    uint8_t* d = dst + i * 4;
                    d[0] = p[0]; d[1] = p[1]; d[2] = p[2]; d[3] = 255;
                }
                break;
            case PixelFormat::BGRA:
                for (size_t i = 0; i < n; ++i)
                {
                    const uint8_t* p = src + i * 4;
                    uint8_t* d = dst + i * 4;
                    d[0] = p[2]; d[1] = p[1]; d[2] = p[0]; d[3] = p[3];
                }
                break;
            case PixelFormat::BGRX:
                for (size_t i = 0; i < n; ++i)
                {
                    const uint8_t* p = src + i * 4;
                    uint8_t* d = dst + i * 4;
                    d[0] = p[2]; d[1] = p[1]; d[2] = p[0]; d[3] = 255;
                }
                break;
        }
        return;
    }

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
    m_fillShader = std::make_unique<Shader>();
    m_fillShader->CompileProgram(header + kVideoPreprocessVertexShader,
                                 header + kVideoFillFragmentShader);
    m_matteShader = std::make_unique<Shader>();
    m_matteShader->CompileProgram(header + kVideoPreprocessVertexShader,
                                  header + kVideoMatteFragmentShader);
    m_temporalShader = std::make_unique<Shader>();
    m_temporalShader->CompileProgram(header + kVideoPreprocessVertexShader,
                                     header + kVideoTemporalFragmentShader);
    m_featherShader = std::make_unique<Shader>();
    m_featherShader->CompileProgram(header + kVideoPreprocessVertexShader,
                                    header + kVideoFeatherFragmentShader);
    m_maskGatherShader = std::make_unique<Shader>();
    m_maskGatherShader->CompileProgram(header + kVideoPreprocessVertexShader,
                                       header + kVideoMaskGatherFragmentShader);
    m_maskBlurShader = std::make_unique<Shader>();
    m_maskBlurShader->CompileProgram(header + kVideoPreprocessVertexShader,
                                     header + kVideoMaskBlurFragmentShader);

    // Mask buffer: 2D RGBA16F sibling of the video texture, sampled by presets as "mask".
    // Same resolution as the processing surfaces (already a downscale of the source).
    m_maskTexture = std::make_shared<Texture>("mask", GL_TEXTURE_2D,
                                              m_texWidth, m_texHeight, 0,
                                              GL_RGBA16F, GL_RGBA, GL_FLOAT, false);
    // Non-mipmap filtering so the gather pass's raw self-read (motion-decay) is texture-complete.
    // Preset sampling overrides these via the TextureManager "fc_" sampler object regardless.
    glBindTexture(GL_TEXTURE_2D, m_maskTexture->TextureID());
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindTexture(GL_TEXTURE_2D, 0);

    // Gate texture: coarse app-supplied alpha weight map (R8). Linear so the shader's fetch
    // feathers it; 1x1 white until the app submits one, so an unsampled gate is a no-op.
    const uint8_t gateInit = 255;
    glGenTextures(1, &m_gateTex);
    glBindTexture(GL_TEXTURE_2D, m_gateTex);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R8, 1, 1, 0, GL_RED, GL_UNSIGNED_BYTE, &gateInit);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    m_gateW = 1;
    m_gateH = 1;

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
    uint32_t* pingPongTextures[8] = {&m_prevTex[0], &m_prevTex[1], &m_bgTex[0], &m_bgTex[1],
                                     &m_morphTex[0], &m_morphTex[1], &m_stableTex[0], &m_stableTex[1]};
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

    // Fullscreen triangle backed by a real VBO + enabled attribute. Apple's OpenGL core
    // profile rejects attributeless (gl_VertexID-only) draws with GL_INVALID_OPERATION, so
    // every preprocess/refine pass must source its vertices from this buffer.
    glGenVertexArrays(1, &m_vao);
    glBindVertexArray(m_vao);
    glGenBuffers(1, &m_vbo);
    glBindBuffer(GL_ARRAY_BUFFER, m_vbo);
    const float triangle[6] = {-1.0f, -1.0f, 3.0f, -1.0f, -1.0f, 3.0f};
    glBufferData(GL_ARRAY_BUFFER, sizeof(triangle), triangle, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, nullptr);
    glBindVertexArray(0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
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

float VideoTexture::BufferSeconds() const
{
    if (m_frameCount <= 1 || m_secondsPerSlice <= 0.0)
    {
        return 0.0f;
    }
    const uint32_t filled = std::min<uint32_t>(m_frameCount - 1,
                                               static_cast<uint32_t>(m_depth - 1));
    return static_cast<float>(static_cast<double>(filled) * m_secondsPerSlice);
}

} // namespace Renderer
} // namespace libprojectM