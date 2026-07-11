/**
 * @file screenshot.hpp
 * @brief Capture the rendered frame to a PNG.
 *
 * Used both interactively (F12) and headlessly: set PROJECTM_SCREENSHOT_AT to have the
 * app photograph itself at fixed times and exit, which makes a preset's look reviewable
 * without a human watching the window at the right moment.
 */
#pragma once

#include <string>

/**
 * @brief Writes the current OpenGL back buffer to a PNG file.
 *
 * Must be called with a current GL context and BEFORE the buffer swap, since it reads
 * the back buffer (that is where the finished frame lives at that point).
 *
 * @param path Destination .png path. Parent directory must exist.
 * @param width Framebuffer width in pixels.
 * @param height Framebuffer height in pixels.
 * @return True if the file was written.
 */
bool saveScreenshotPng(const std::string& path, int width, int height);

/**
 * @brief Writes an OpenGL 2D texture to a PNG file.
 *
 * Used to capture the preset's PRE-COMPOSITE image (see projectm_opengl_get_main_texture): the
 * drawing the warp/pattern stage produced, before the composite shader crops, curves, shades or
 * frames it. The composite can make the final output look nothing like the drawing, so when the
 * question is "what is the warp shader doing?", this is the surface to look at -- and when the
 * question is "what does the viewer see?", the window capture above is.
 *
 * @param path Destination .png path.
 * @param textureId The GL texture id to read (0 is a no-op).
 * @return True if the file was written.
 */
bool saveTexturePng(const std::string& path, unsigned int textureId);
