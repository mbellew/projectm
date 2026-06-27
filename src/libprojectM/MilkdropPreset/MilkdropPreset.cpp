/**
 * projectM -- Milkdrop-esque visualisation SDK
 * Copyright (C)2003-2004 projectM Team
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

#include "MilkdropPreset.hpp"

#include "Factory.hpp"
#include "MilkdropPresetExceptions.hpp"
#include "PresetFileParser.hpp"

#include <Logging.hpp>
#include <Renderer/VideoTexture.hpp>

namespace libprojectM {
namespace MilkdropPreset {

MilkdropPreset::MilkdropPreset(const std::string& absoluteFilePath)
    : m_absoluteFilePath(absoluteFilePath)
    , m_perFrameContext(m_state.globalMemory, &m_state.globalRegisters, &m_state.palette)
    , m_perPixelContext(m_state.globalMemory, &m_state.globalRegisters, &m_state.palette)
    , m_motionVectors(m_state)
    , m_waveform(m_state)
    , m_darkenCenter(m_state)
    , m_border(m_state)
{
    Load(absoluteFilePath);
}

MilkdropPreset::MilkdropPreset(std::istream& presetData)
    : m_perFrameContext(m_state.globalMemory, &m_state.globalRegisters, &m_state.palette)
    , m_perPixelContext(m_state.globalMemory, &m_state.globalRegisters, &m_state.palette)
    , m_motionVectors(m_state)
    , m_waveform(m_state)
    , m_darkenCenter(m_state)
    , m_border(m_state)
{
    Load(presetData);
}

void MilkdropPreset::Initialize(const Renderer::RenderContext& renderContext)
{
    assert(renderContext.textureManager);
    m_state.renderContext = renderContext;

    // Resolve the color palette now that the search paths are available (PALETTE_NAME may name an
    // image file). Fixed for the preset's lifetime; feeds palette_r/g/b (eval) and sampler_fc_palette.
    {
        static const std::vector<std::string> noPaths;
        const std::vector<std::string>& palettePaths =
            renderContext.paletteSearchPaths != nullptr ? *renderContext.paletteSearchPaths : noPaths;
        m_state.palette.Resolve(m_state.paletteName, m_state.paletteSmoothH, m_state.paletteSmoothV, palettePaths);
    }

    m_state.blurTexture.Initialize(renderContext);
    m_state.LoadShaders();

    // Initialize variables and code now we have a proper render state.
    CompileCodeAndRunInitExpressions();

    // Update framebuffer and texture sizes if needed
    m_framebuffer.SetSize(renderContext.viewportSizeX, renderContext.viewportSizeY);
    m_motionVectorUVMap->SetSize(renderContext.viewportSizeX, renderContext.viewportSizeY);
    if (m_state.mainTexture.expired())
    {
        m_state.mainTexture = m_framebuffer.GetColorAttachmentTexture(1, 0);
    }

    m_perPixelMesh.CompileWarpShader(m_state);
    m_finalComposite.CompileCompositeShader(m_state);

    if (m_videoShader)
    {
        try
        {
            m_videoShader->CompileVideoShader();
            LOG_DEBUG("[MilkdropPreset] Successfully compiled video shader code.");
        }
        catch (const Renderer::ShaderException& ex)
        {
            LOG_WARN("[MilkdropPreset] Error compiling video shader code: " + ex.message() + " - Falling back to the fixed video_alpha_mode path.");
            m_videoShader.reset();
        }
    }

    /*FLOATBUF*/
    // Initialize the per-pixel state stored in the pattern buffer's alpha channel. Unless the
    // preset opts into carryover (inherit whatever state is already there), clear only the A
    // channel of both pattern surfaces to fAlphaInit (default 1.0), leaving RGB untouched.
    if (!m_state.alphaCarryover)
    {
        m_framebuffer.ClearColorChannels(m_currentFrameBuffer, false, false, false, true, m_state.alphaInit);
        m_framebuffer.ClearColorChannels(m_previousFrameBuffer, false, false, false, true, m_state.alphaInit);
    }
}

void MilkdropPreset::RenderFrame(const libprojectM::Audio::FrameAudioData& audioData, const Renderer::RenderContext& renderContext)
{
    m_state.audioData = audioData;
    m_state.renderContext = renderContext;

    // Update framebuffer and u/v texture size if needed
    if (m_framebuffer.SetSize(renderContext.viewportSizeX, renderContext.viewportSizeY))
    {
        m_motionVectorUVMap->SetSize(renderContext.viewportSizeX, renderContext.viewportSizeY);
        m_isFirstFrame = true;
    }

    m_state.mainTexture = m_framebuffer.GetColorAttachmentTexture(m_previousFrameBuffer, 0);

    // First evaluate per-frame code
    PerFrameUpdate();

    glViewport(0, 0, renderContext.viewportSizeX, renderContext.viewportSizeY);

    m_framebuffer.Bind(m_previousFrameBuffer);
    // Motion vector field. Drawn to the previous frame texture before warping it.
    // Only do it after drawing one frame after init or resize.
    if (!m_isFirstFrame)
    {
        m_motionVectors.Draw(m_perFrameContext, m_motionVectorUVMap->Texture());
    }

    // y-flip the previous frame and assign the flipped texture as "main"
    m_flipTexture.Draw(*renderContext.shaderCache, m_framebuffer.GetColorAttachmentTexture(m_previousFrameBuffer, 0), nullptr, true, false);
    m_state.mainTexture = m_flipTexture.Texture();

    // We now draw to the current framebuffer.
    m_framebuffer.Bind(m_currentFrameBuffer);

    // Add motion vector u/v texture for the warp mesh draw and clean both buffers.
    m_framebuffer.SetAttachment(m_currentFrameBuffer, 1, m_motionVectorUVMap);

    // Draw previous frame image warped via per-pixel mesh and warp shader
    m_perPixelMesh.Draw(m_state, m_perFrameContext, m_perPixelContext);

    // Remove the u/v texture from the framebuffer.
    m_framebuffer.RemoveColorAttachment(m_currentFrameBuffer, 1);

    // Update blur textures
    {
        const auto warpedImage = m_framebuffer.GetColorAttachmentTexture(m_previousFrameBuffer, 0);
        assert(warpedImage.get());
        m_state.blurTexture.Update(*warpedImage, m_perFrameContext);
    }

    // Draw audio-data-related stuff
    if (m_state.shapesEnabled)
    {
        for (auto& shape : m_customShapes)
        {
            shape->Draw();
        }
    }
    for (auto& wave : m_customWaveforms)
    {
        wave->Draw(m_perFrameContext);
    }
    if (m_state.waveEnabled)
    {
        m_waveform.Draw(m_perFrameContext);
    }

    // Done in DrawSprites() in Milkdrop
    if (*m_perFrameContext.darken_center > 0)
    {
        m_darkenCenter.Draw();
    }
    m_border.Draw(m_perFrameContext);

    // y-flip the image for final compositing again
    m_flipTexture.Draw(*renderContext.shaderCache, m_framebuffer.GetColorAttachmentTexture(m_currentFrameBuffer, 0), nullptr, true, false);
    m_state.mainTexture = m_flipTexture.Texture();

    // We no longer need the previous frame image, use it to render the final composite.
    m_framebuffer.BindRead(m_currentFrameBuffer);
    m_framebuffer.BindDraw(m_previousFrameBuffer);

    m_finalComposite.Draw(m_state, m_perFrameContext);

    if (!m_finalComposite.HasCompositeShader())
    {
        // Flip texture again in "previous" framebuffer as old-school effects are still upside down.
        m_flipTexture.Draw(*renderContext.shaderCache, m_framebuffer.GetColorAttachmentTexture(m_previousFrameBuffer, 0), m_framebuffer, m_previousFrameBuffer, true, false);
    }

    // Swap framebuffer IDs for the next frame.
    std::swap(m_currentFrameBuffer, m_previousFrameBuffer);

    m_isFirstFrame = false;
}

auto MilkdropPreset::OutputTexture() const -> std::shared_ptr<Renderer::Texture>
{
    // the composited image is always stored in the "current" framebuffer after a frame is rendered.
    return m_framebuffer.GetColorAttachmentTexture(m_currentFrameBuffer, 0);
}

auto MilkdropPreset::IsComplete() const -> bool
{
    return m_presetComplete;
}

void MilkdropPreset::DrawInitialImage(const std::shared_ptr<Renderer::Texture>& image, const Renderer::RenderContext& renderContext)
{
    m_framebuffer.SetSize(renderContext.viewportSizeX, renderContext.viewportSizeY);

    // Render to previous framebuffer, as this is the image used to draw the next frame on.
    m_flipTexture.Draw(*renderContext.shaderCache, image, m_framebuffer, m_previousFrameBuffer);
}

void MilkdropPreset::BindFramebuffer()
{
    if (m_framebuffer.Width() > 0 && m_framebuffer.Height() > 0)
    {
        m_framebuffer.BindDraw(m_previousFrameBuffer);
    }
}

void MilkdropPreset::PerFrameUpdate()
{
    m_perFrameContext.LoadStateVariables(m_state);
    m_perPixelContext.LoadStateReadOnlyVariables(m_state, m_perFrameContext);

    m_perFrameContext.ExecutePerFrameCode();

    // Let the preset request a switch (self-capped duration, end-on-cue, etc.).
    m_presetComplete = (*m_perFrameContext.preset_complete > 0.5);

    m_perPixelContext.LoadPerFrameQVariables(m_state, m_perFrameContext);

    // Clamp gamma and echo zoom values
    *m_perFrameContext.gamma = std::max(0.0, std::min(8.0, *m_perFrameContext.gamma));
    *m_perFrameContext.echo_zoom = std::max(0.001, std::min(1000.0, *m_perFrameContext.echo_zoom));

    // Upload any pending video frame using preset-controlled alpha parameters,
    // then refresh shader-visible ring-buffer state in renderContext.
    if (m_state.renderContext.videoTexture != nullptr)
    {
        int mode = static_cast<int>(*m_perFrameContext.video_alpha_mode);
        if (mode < 0) mode = 0;
        if (mode > 5) mode = 5;

        Renderer::VideoTexture::AlphaParams params;
        params.mode = static_cast<Renderer::VideoTexture::AlphaMode>(mode);
        params.value = static_cast<float>(*m_perFrameContext.video_alpha_value);
        params.init = static_cast<float>(*m_perFrameContext.video_alpha_init);
        params.decay = static_cast<float>(*m_perFrameContext.video_alpha_decay);
        params.cleanup = static_cast<int>(*m_perFrameContext.video_cleanup);
        params.refine = *m_perFrameContext.video_refine > 0.5;
        // A preset video_ shader (if present and compiled) authors the alpha/rgb written into the
        // history, superseding the fixed alpha-mode path for this frame.
        Renderer::Shader* alphaShader = m_videoShader ? &m_videoShader->Shader() : nullptr;
        m_state.renderContext.videoTexture->UpdateGPU(params, alphaShader);

        m_state.renderContext.videoZWrite = m_state.renderContext.videoTexture->NormalizedWritePosition();
        m_state.renderContext.videoZRange = m_state.renderContext.videoTexture->NormalizedRange();
        m_state.renderContext.videoFrameCount = static_cast<float>(m_state.renderContext.videoTexture->FrameCount());
        m_state.renderContext.videoBufferSeconds = m_state.renderContext.videoTexture->BufferSeconds();
    }
}

void MilkdropPreset::Load(const std::string& pathname)
{
    LOG_DEBUG("[MilkdropPreset] Loading preset from file \"" + pathname + "\".")

    SetFilename(ParseFilename(pathname));

    PresetFileParser parser;

    if (!parser.Read(pathname))
    {
        const std::string error = "[MilkdropPreset] Could not parse preset file \"" + pathname + "\".";
        LOG_ERROR(error)
        throw MilkdropPresetLoadException(error);
    }

    InitializePreset(parser);
}

void MilkdropPreset::Load(std::istream& stream)
{
    LOG_DEBUG("[MilkdropPreset] Loading preset from stream.");

    PresetFileParser parser;

    if (!parser.Read(stream))
    {
        const std::string error =  "[MilkdropPreset] Could not parse preset data.";
        LOG_ERROR(error)
        throw MilkdropPresetLoadException(error);
    }

    InitializePreset(parser);
}

void MilkdropPreset::InitializePreset(PresetFileParser& parsedFile)
{
    // Create the offscreen rendering surfaces.
    m_motionVectorUVMap = std::make_shared<Renderer::TextureAttachment>(GL_RG16F, GL_RG, GL_FLOAT, 0, 0);
    // FLOATBUF: float main pattern buffer so the alpha channel can hold per-pixel state that
    // follows the pixels through the warp feedback. Switch to GL_RGBA16F to halve memory.
    m_framebuffer.CreateColorAttachment(0, 0, GL_RGBA16F, GL_RGBA, GL_FLOAT); /*FLOATBUF*/ // Main image 1
    m_framebuffer.CreateColorAttachment(1, 0, GL_RGBA16F, GL_RGBA, GL_FLOAT); /*FLOATBUF*/ // Main image 2

    Renderer::Framebuffer::Unbind();

    // Load global init variables into the state
    m_state.Initialize(parsedFile);

    // Register code context variables
    m_perFrameContext.RegisterBuiltinVariables();
    m_perPixelContext.RegisterBuiltinVariables();

    // Custom waveforms:
    for (int i = 0; i < CustomWaveformCount; i++)
    {
        auto wave = std::make_unique<CustomWaveform>(m_state);
        wave->Initialize(parsedFile, i);
        m_customWaveforms[i] = std::move(wave);
    }

    // Custom shapes:
    for (int i = 0; i < CustomShapeCount; i++)
    {
        auto shape = std::make_unique<CustomShape>(m_state);
        shape->Initialize(parsedFile, i);
        m_customShapes[i] = std::move(shape);
    }

    // Preload shaders
    LoadShaderCode();
}

void MilkdropPreset::CompileCodeAndRunInitExpressions()
{
    // Per-frame init and code
    m_perFrameContext.LoadStateVariables(m_state);
    m_perFrameContext.EvaluateInitCode(m_state);
    m_perFrameContext.CompilePerFrameCode(m_state.perFrameCode);

    // Per-vertex code
    m_perPixelContext.CompilePerPixelCode(m_state.perPixelCode);

    for (int i = 0; i < CustomWaveformCount; i++)
    {
        auto& wave = m_customWaveforms[i];
        wave->CompileCodeAndRunInitExpressions(m_perFrameContext);
    }

    for (int i = 0; i < CustomShapeCount; i++)
    {
        auto& shape = m_customShapes[i];
        shape->CompileCodeAndRunInitExpressions();
    }
}

void MilkdropPreset::LoadShaderCode()
{
    m_perPixelMesh.LoadWarpShader(m_state);
    m_finalComposite.LoadCompositeShader(m_state);

    // Optional video_ shader: gated on the same version flag as warp/comp (shaders enabled at
    // all). Only the HLSL->GLSL string prep happens here; the GL compile is deferred to
    // Initialize() where a GL context is current (see CompileVideoShader).
    m_videoShader.reset();
    if (m_state.compositeShaderVersion > 0 && !m_state.videoShader.empty())
    {
        try
        {
            m_videoShader = std::make_unique<MilkdropShader>(MilkdropShader::ShaderType::VideoShader);
            m_videoShader->LoadCode(m_state.videoShader);
            LOG_DEBUG("[MilkdropPreset] Successfully loaded video shader code.");
        }
        catch (const Renderer::ShaderException& ex)
        {
            LOG_WARN("[MilkdropPreset] Error loading video shader code: " + ex.message() + " - Falling back to the fixed video_alpha_mode path.");
            m_videoShader.reset();
        }
    }
}

auto MilkdropPreset::ParseFilename(const std::string& filename) -> std::string
{
    const std::size_t start = filename.find_last_of('/');

    if (start == std::string::npos || start >= (filename.length() - 1))
    {
        return "";
    }

    return filename.substr(start + 1, filename.length());
}


} // namespace MilkdropPreset
} // namespace libprojectM
