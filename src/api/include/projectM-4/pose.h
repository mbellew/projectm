/**
 * @file pose.h
 * @copyright 2003-2025 projectM Team
 * @brief Body-pose input: hand a skeleton to projectM so presets can read joints.
 * @since 4.2.0
 *
 * The application runs whatever pose tracker it likes and submits a skeleton once per frame with
 * projectm_pose_set(). projectM smooths it, derives per-joint velocity, computes a few robust
 * derived scalars, and exposes it all to preset code as the pose(JOINT, VAR) eval function.
 *
 * Coordinate convention (identical to the seg_* and touch_* surfaces, so they interoperate):
 *   x in [0,1], left to right
 *   y in [0,1], BOTTOM to top
 *   z in [0,1] closeness (1 = nearest), or negative when no depth is available
 *   confidence in [0,1]; 0 means "not detected this frame"
 * Submit camera-native coordinates; projectM applies the mirror (projectm_video_set_mirror).
 *
 * See POSE_API.md for the full design.
 *
 * projectM -- Milkdrop-esque visualisation SDK
 * Copyright (C)2003-2024 projectM Team
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
 */

#pragma once

#include "projectM-4/types.h"

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Joint indices. 0..16 are the COCO-17 keypoints most 2D pose models emit; 17..21 are
 *        derived points the raw skeleton does not contain but presets actually want.
 *
 * These values are the ones preset code sees as the ALL_CAPS constants (NOSE, R_WRIST, HEART, ...).
 * @since 4.2.0
 */
typedef enum
{
    PROJECTM_JOINT_NOSE = 0,
    PROJECTM_JOINT_L_EYE = 1,
    PROJECTM_JOINT_R_EYE = 2,
    PROJECTM_JOINT_L_EAR = 3,
    PROJECTM_JOINT_R_EAR = 4,
    PROJECTM_JOINT_L_SHOULDER = 5,
    PROJECTM_JOINT_R_SHOULDER = 6,
    PROJECTM_JOINT_L_ELBOW = 7,
    PROJECTM_JOINT_R_ELBOW = 8,
    PROJECTM_JOINT_L_WRIST = 9,
    PROJECTM_JOINT_R_WRIST = 10,
    PROJECTM_JOINT_L_HIP = 11,
    PROJECTM_JOINT_R_HIP = 12,
    PROJECTM_JOINT_L_KNEE = 13,
    PROJECTM_JOINT_R_KNEE = 14,
    PROJECTM_JOINT_L_ANKLE = 15,
    PROJECTM_JOINT_R_ANKLE = 16,

    //! Chest/heart: shoulder midpoint dropped toward the hips. The body's visual center.
    PROJECTM_JOINT_HEART = 17,
    //! Hand tips, extrapolated past the wrist along the forearm (COCO-17 stops at the wrist).
    PROJECTM_JOINT_L_HAND = 18,
    PROJECTM_JOINT_R_HAND = 19,
    //! Head center (above the nose); anchor for halo/crown/head-warp.
    PROJECTM_JOINT_HEAD = 20,
    //! Hip midpoint; body root.
    PROJECTM_JOINT_PELVIS = 21,

    PROJECTM_JOINT_COUNT = 22
} projectm_pose_joint_index;

/**
 * @brief One joint as submitted by the application. Velocity is derived by projectM.
 * @since 4.2.0
 */
typedef struct
{
    float x;          //!< [0,1], left to right (camera-native; projectM mirrors).
    float y;          //!< [0,1], bottom to top.
    float z;          //!< [0,1] closeness (1 = nearest); negative if unavailable.
    float confidence; //!< [0,1]; 0 = not detected.
} projectm_pose_joint;

/**
 * @brief Submits the current skeleton. Safe to call from any thread (e.g. a capture thread).
 *
 * Call once per captured frame. projectM smooths the joints, finite-differences velocity, and
 * updates the derived scalars. Joints with confidence 0 hold their last confident position while
 * their confidence decays, so anything a preset anchors to a joint does not snap away when the
 * tracker briefly loses it.
 *
 * Passing count 0 (or never calling this) simply means no pose: pose_valid stays 0 and every
 * joint reports confidence 0.
 *
 * @param instance The projectM instance handle.
 * @param joints Array of joints, indexed by projectm_pose_joint_index.
 * @param count Number of entries in @p joints, normally PROJECTM_JOINT_COUNT. Extra entries are
 *              ignored; missing trailing entries are treated as not detected.
 * @since 4.2.0
 */
PROJECTM_EXPORT void projectm_pose_set(projectm_handle instance,
                                       const projectm_pose_joint* joints, size_t count);

#ifdef __cplusplus
} // extern "C"
#endif
