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
