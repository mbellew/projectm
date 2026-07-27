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
    PROJECTM_VIDEO_FORMAT_RGBX = 3, //!< 4 bytes per pixel, alpha byte ignored and treated as opaque (e.g. a camera).
    PROJECTM_VIDEO_FORMAT_BGRX = 4, //!< 4 bytes per pixel, red/blue swapped, alpha byte ignored and treated as opaque.
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
 * @brief Submits a coarse alpha weight map ("gate") multiplied into the app-supplied matte.
 *
 * An application that derives a foreground matte may separately decide how much of that matte to
 * keep per region -- e.g. a depth or pose gate that fades out background people. Submitting the
 * decision as a low-resolution grid lets projectM apply it during GPU preprocessing, instead of
 * the application multiplying it into every pixel of every frame on the capture thread. The grid
 * is sampled bilinearly, which feathers it exactly as a CPU bilinear apply would.
 *
 * Weights are in [0,1] (1 = keep the matte unchanged, 0 = remove it), row-major, and in the same
 * un-mirrored space as the submitted frame. The map persists until replaced, so resubmit it
 * whenever it changes. Safe to call from any thread. Applies to frames submitted with
 * projectm_video_submit_frame(); frames submitted with projectm_video_submit_frame_gpu() already
 * carry a finished mask and are not gated. If projectm_video_configure() has not been called,
 * this is a no-op.
 *
 * @param instance The projectM instance handle.
 * @param weights Pointer to grid_width * grid_height floats in [0,1], row-major.
 * @param grid_width Grid width in cells.
 * @param grid_height Grid height in cells.
 * @since 4.2.0
 */
PROJECTM_EXPORT void projectm_video_submit_alpha_gate(projectm_handle instance,
                                                      const float* weights,
                                                      unsigned int grid_width,
                                                      unsigned int grid_height);

/**
 * @brief Returns the GL texture name of the RGBA8 input surface for GPU preprocessing.
 *
 * Applications that preprocess frames on the GPU (e.g. a depth camera compositing a real
 * foreground mask into alpha) render their finished RGBA frame into this texture and then
 * call projectm_video_submit_frame_gpu(). The texture is sized to the configured slice
 * dimensions (tex_width x tex_height). Returns 0 if projectm_video_configure() has not been
 * called. Must be called on the thread with the current GL context.
 *
 * @param instance The projectM instance handle.
 * @return The GL texture name, or 0 if video is not configured.
 * @since 4.2.0
 */
PROJECTM_EXPORT unsigned int projectm_video_get_input_texture(projectm_handle instance);

/**
 * @brief Submits a frame the application has rendered into the GPU input texture.
 *
 * The next call to projectm_render_frame() copies the input texture (see
 * projectm_video_get_input_texture()) into the next ring-buffer slice verbatim: RGB as
 * drawn and alpha taken as the application-supplied mask. The preset's alpha mode and mask
 * cleanup are bypassed for GPU-submitted frames. Must be called on the GL thread, after
 * rendering into the input texture and before projectm_render_frame(). If
 * projectm_video_configure() has not been called, this is a no-op.
 *
 * @param instance The projectM instance handle.
 * @since 4.2.0
 */
PROJECTM_EXPORT void projectm_video_submit_frame_gpu(projectm_handle instance);

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
 * @brief Horizontally mirrors incoming video frames during preprocessing.
 *
 * Applies to every alpha mode and is off by default. If projectm_video_configure() has not been
 * called, this is a no-op.
 *
 * @param instance The projectM instance handle.
 * @param mirror True to mirror left/right, false to leave frames as-is.
 * @since 4.2.0
 */
PROJECTM_EXPORT void projectm_video_set_mirror(projectm_handle instance, bool mirror);

/**
 * @brief Sets an application-global foreground-masking override.
 *
 * Foreground extraction (background subtraction, chroma key, depth/person masks) is usually a
 * scene/hardware property the application owns rather than the preset. When @p mode is >= 0 it
 * overrides the preset's per-frame alpha mode and runs the library's masking pipeline; when
 * @p mode is < 0 (the default) masking stays under preset control. @p refine enables the shared
 * refinement back-end (guided fill, matte, temporal stabilization, feather) on top of the prior.
 *
 * Mode values match the library's alpha modes: 0 Source, 1 Constant, 2 Motion, 3 MotionDecay,
 * 4 ChromaKey, 5 BackgroundSubtract.
 *
 * @param instance The projectM instance handle.
 * @param mode Alpha-mode value to force, or -1 to defer to the preset.
 * @param refine Run the refinement back-end when the override is active.
 * @since 4.2.0
 */
PROJECTM_EXPORT void projectm_video_set_mask_mode(projectm_handle instance, int mode, bool refine);

/**
 * @brief Returns true if the video-history texture has been configured.
 *
 * @param instance The projectM instance handle.
 * @return True if projectm_video_configure() has been called successfully.
 * @since 4.2.0
 */
PROJECTM_EXPORT bool projectm_video_is_active(projectm_handle instance);

/**
 * @brief Supplies the person-segmentation centroid for the current frame.
 *
 * The application computes the centroid of its foreground matte and passes it here. The
 * library smooths it and exposes it to presets as the eval variables and shader uniforms
 * seg_cx, seg_cy, seg_vx, seg_vy, seg_coverage and seg_valid. Velocity is derived by the
 * library from the centroid and its frame timing. When coverage is low or no update arrives
 * for a short while, the centroid eases back to screen center (0.5, 0.5) and seg_valid goes 0.
 *
 * Horizontal mirroring (projectm_video_set_mirror) is applied internally, so pass the centroid
 * in the camera-native orientation. Safe to call from any thread.
 *
 * @param instance The projectM instance handle.
 * @param cx Centroid X in [0,1], left to right.
 * @param cy Centroid Y in [0,1], bottom to top (matches preset per-pixel y).
 * @param coverage Foreground fraction of the frame, [0,1].
 * @since 4.3.0
 */
PROJECTM_EXPORT void projectm_video_set_seg_centroid(projectm_handle instance,
                                                     float cx, float cy, float coverage);

/**
 * @brief Submits the main figure's exposure verdict (from an app-side NudeNet detector).
 *
 * The values are expected to be already de-flickered by the caller (near-binary, covered/0 by
 * default); projectM stores them verbatim and exposes them to presets as the nude_* eval
 * variables (nude_top, nude_rear, nude_front_f, nude_front_m, nude_female). Safe to call from
 * any thread. Unlike the centroid, these are states rather than coordinates, so mirroring does
 * not apply.
 *
 * @param instance The projectM instance handle.
 * @param top Bare chest of the main figure, [0,1] (nude_top).
 * @param rear Bare buttocks, [0,1] (nude_rear).
 * @param front_female Exposed female genitalia, [0,1] (nude_front_f).
 * @param front_male Exposed male genitalia, [0,1] (nude_front_m).
 * @param female Face gender axis: 1=female, 0=male, 0.5=unknown (nude_female).
 * @since 4.3.0
 */
PROJECTM_EXPORT void projectm_video_set_nudity(projectm_handle instance, float top, float rear,
                                               float front_female, float front_male, float female);

#ifdef __cplusplus
} // extern "C"
#endif