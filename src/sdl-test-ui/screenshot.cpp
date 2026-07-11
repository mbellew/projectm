#include "screenshot.hpp"

#include "opengl.h"

#include <SDL2/SDL.h>

#include <vector>

// The vendored stb_image target compiles the decoder but not the writer, so this TU
// provides the writer implementation. Keep it in this file only (single definition).
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include <stb_image_write.h>

bool saveScreenshotPng(const std::string& path, int width, int height)
{
    if (width <= 0 || height <= 0)
    {
        return false;
    }

    const size_t stride = static_cast<size_t>(width) * 4;
    std::vector<unsigned char> pixels(stride * static_cast<size_t>(height));

    // Read the finished frame from the WINDOW's back buffer.
    //
    // Binding the default framebuffer first is essential, not defensive: rendering leaves one of
    // the preset's own framebuffers bound, so a bare glReadBuffer(GL_BACK) + glReadPixels here
    // captures whatever internal surface happens to be current (the preset's pattern/main texture)
    // rather than the composited frame the viewer actually sees. That silently drops the entire
    // composite shader -- for a preset that draws a scene around its pattern (a page on a desk,
    // a border, a vignette), the capture looks nothing like the real output.
    GLint prevFbo = 0;
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &prevFbo);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    glPixelStorei(GL_PACK_ALIGNMENT, 1);
    glReadBuffer(GL_BACK);
    glReadPixels(0, 0, width, height, GL_RGBA, GL_UNSIGNED_BYTE, pixels.data());

    glBindFramebuffer(GL_FRAMEBUFFER, static_cast<GLuint>(prevFbo));

    // OpenGL's origin is bottom-left, PNG's is top-left: tell stb to flip on write
    // rather than shuffling rows ourselves.
    stbi_flip_vertically_on_write(1);

    const int ok = stbi_write_png(path.c_str(), width, height, 4, pixels.data(),
                                  static_cast<int>(stride));
    if (ok == 0)
    {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "[screenshot] Failed to write %s", path.c_str());
        return false;
    }

    SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, "[screenshot] Wrote %s (%dx%d)", path.c_str(), width, height);
    return true;
}

bool saveTexturePng(const std::string& path, unsigned int textureId)
{
    if (textureId == 0)
    {
        return false;
    }

    glBindTexture(GL_TEXTURE_2D, textureId);

    // Ask the texture for its own size rather than assuming the window's: the preset renders at
    // the viewport size, but that is not guaranteed to match (supersampling, resize races).
    GLint width = 0;
    GLint height = 0;
    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &width);
    glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, &height);
    if (width <= 0 || height <= 0)
    {
        glBindTexture(GL_TEXTURE_2D, 0);
        return false;
    }

    const size_t stride = static_cast<size_t>(width) * 4;
    std::vector<unsigned char> pixels(stride * static_cast<size_t>(height));

    // The preset's surfaces are float (RGBA16F); ask GL to convert to 8-bit on read.
    glPixelStorei(GL_PACK_ALIGNMENT, 1);
    glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_BYTE, pixels.data());
    glBindTexture(GL_TEXTURE_2D, 0);

    stbi_flip_vertically_on_write(1);
    const int ok = stbi_write_png(path.c_str(), width, height, 4, pixels.data(),
                                  static_cast<int>(stride));
    if (ok == 0)
    {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "[screenshot] Failed to write %s", path.c_str());
        return false;
    }

    SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, "[screenshot] Wrote %s (%dx%d, main texture)",
                path.c_str(), width, height);
    return true;
}
