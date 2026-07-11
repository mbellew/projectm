#pragma once

#include <Audio/FrameAudioData.hpp>

#include <Renderer/RenderContext.hpp>
#include <Renderer/Texture.hpp>

#include <memory>
#include <string>

namespace libprojectM {

class Preset
{
public:
    virtual ~Preset() = default;

    /**
     * @brief Initializes additional preset resources.
     * @param renderContext A render context with the initial data.
     */
    virtual void Initialize(const Renderer::RenderContext& renderContext) = 0;

    /**
     * @brief Renders the preset into the current framebuffer.
     * @param audioData Audio data to be used by the preset.
     * @param renderContext The current render context data.
     */
    virtual void RenderFrame(const libprojectM::Audio::FrameAudioData& audioData,
                             const Renderer::RenderContext& renderContext) = 0;

    /**
     * @brief Returns a pointer to the current rendering output texture.
     * This pointer (the actual texture) may change from frame to frame, so this pointer should not be stored for use
     * across multiple frames. Instead, a new pointer should be requested whenever needed.
     * @return A pointer to the current output texture of the preset.
     */
    virtual auto OutputTexture() const -> std::shared_ptr<Renderer::Texture> = 0;

    /**
     * @brief Returns the preset's PRE-COMPOSITE image: the surface the composite shader reads,
     * and the same image that is fed back in as "sampler_main" on the next frame.
     *
     * This is what the warp shader actually draws, before the composite stage rearranges it (a
     * composite shader can crop, curve, shade or frame the image -- so the final output can look
     * nothing like the drawing itself). Useful for inspecting warp/pattern work in isolation.
     *
     * Defaults to the composited output for preset types that have no separate surface.
     * @return A pointer to the preset's pre-composite ("main") texture.
     */
    virtual auto MainTexture() const -> std::shared_ptr<Renderer::Texture> { return OutputTexture(); }

    /**
     * @brief Draws an initial image into the preset, e.g. the last frame of a previous preset.
     * It's not guaranteed a preset supports using a previously rendered image. If not
     * supported, this call is simply a no-op.
     * @param image The texture to be copied as the initial preset image.
     * @param renderContext The current render context data.
     */
    virtual void DrawInitialImage(const std::shared_ptr<Renderer::Texture>& image,
                                  const Renderer::RenderContext& renderContext) = 0;

    /**
     * @brief Bind the preset's internal framebuffer.
     * This framebuffer contains the image passed on to the next frame, onto which warp effects etc.
     * are then applied. Has no effect if the framebuffer isn't initialized yet, e.g. before drawing
     * the first frame.
     *
     * Can be used to draw anything on top of the preset's image, "burning in" additional shapes,
     * images or text. Depending on the preset, it's not guaranteed though that the image actually
     * is used in the next frame, or completely painted over. That said, the effect varies between
     * presets.
     */
    virtual void BindFramebuffer() = 0;

    /**
     * @brief Whether the preset has signalled it wants projectM to switch away from it.
     * Milkdrop presets set this via the writable per-frame variable `preset_complete` (> 0.5),
     * letting a preset cap its own duration or end on a cue. Defaults to false.
     */
    virtual auto IsComplete() const -> bool { return false; }

    inline void SetFilename(const std::string& filename)
    {
        m_filename = filename;
    }

    inline auto Filename() const -> const std::string&
    {
        return m_filename;
    }

private:
    std::string m_filename;
};

} // namespace libprojectM
