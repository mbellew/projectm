/**
 * @file video.h
 * @copyright 2003-2026 projectM Team
 * @brief Functions to pass video frames into libprojectM for the video-history texture.
 * @since 4.2.0
 *
 * projectM -- Milkdrop-esque visualisation SDK
 * Copyright (C)2003-2026 projectM Team
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
 */

#pragma once

#include "projectM-4/types.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Pixel format of frames passed to projectm_video_submit_frame().
 */
typedef enum
{
    PROJECTM_VIDEO_FORMAT_RGB = 0,  //!< 3 bytes per pixel, alpha defaulted to opaque.
    PROJECTM_VIDEO_FORMAT_RGBA = 1, //!< 4 bytes per pixel.
    PROJECTM_VIDEO_FORMAT_BGRA = 2, //!< 4 bytes per pixel with red/blue swapped.
} projectm_video_format;

/**
 * @brief Allocates the video-history 3D texture.
 *
 * Must be called once with a current GL context before submitting frames. The texture
 * is exposed to presets under the name "video" (e.g. sampler_fw_video). A reasonable
 * default is 256x144x120 (~12.6 MB).
 *
 * @param instance The projectM instance handle.
 * @param tex_width Width in pixels of each slice. Source frames are downscaled to this.
 * @param tex_height Height in pixels of each slice.
 * @param history_depth Number of frames retained in the ring buffer (Z size).
 * @since 4.2.0
 */
PROJECTM_EXPORT void projectm_video_configure(projectm_handle instance,
                                              unsigned int tex_width,
                                              unsigned int tex_height,
                                              unsigned int history_depth);

/**
 * @brief Submits a new video frame to the history buffer.
 *
 * Safe to call from any thread. The frame is downscaled and staged for upload; the
 * next call to projectm_render_frame() uploads it to the next ring-buffer slice.
 * If projectm_video_configure() has not been called, this is a no-op.
 *
 * @param instance The projectM instance handle.
 * @param data Pointer to tightly-packed pixel data in the given format.
 * @param width Source frame width in pixels.
 * @param height Source frame height in pixels.
 * @param format Pixel format of the source data.
 * @since 4.2.0
 */
PROJECTM_EXPORT void projectm_video_submit_frame(projectm_handle instance,
                                                 const void* data,
                                                 unsigned int width,
                                                 unsigned int height,
                                                 projectm_video_format format);

/**
 * @brief Sets the chroma-key background color for the video ChromaKey alpha mode.
 *
 * The key color is scene/camera dependent (the real green-screen color, or the
 * application's virtual-green-screen sentinel), so it is supplied by the application
 * rather than by presets. Components are normalized [0,1]. Defaults to black (0,0,0),
 * which is also the recommended virtual-green-screen sentinel.
 *
 * @param instance The projectM instance handle.
 * @param r Red component, 0..1.
 * @param g Green component, 0..1.
 * @param b Blue component, 0..1.
 * @since 4.2.0
 */
PROJECTM_EXPORT void projectm_video_set_chroma_key(projectm_handle instance,
                                                   float r, float g, float b);

/**
 * @brief Returns true if the video-history texture has been configured.
 *
 * @param instance The projectM instance handle.
 * @return True if projectm_video_configure() has been called successfully.
 * @since 4.2.0
 */
PROJECTM_EXPORT bool projectm_video_is_active(projectm_handle instance);

#ifdef __cplusplus
} // extern "C"
#endif