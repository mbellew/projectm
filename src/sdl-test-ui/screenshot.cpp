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

    // Read the finished frame from the back buffer.
    glPixelStorei(GL_PACK_ALIGNMENT, 1);
    glReadBuffer(GL_BACK);
    glReadPixels(0, 0, width, height, GL_RGBA, GL_UNSIGNED_BYTE, pixels.data());

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
