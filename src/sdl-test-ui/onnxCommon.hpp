/**
 * @file onnxCommon.hpp
 * @brief Small ONNX Runtime helpers shared by the seg and pose backends.
 *
 * Compiled only when ENABLE_ONNX_SEG is on and onnxruntime is found (the same guard
 * as segMask.cpp / poseTracker.cpp). Holds the pieces both backends need verbatim:
 * env-var readers, YOLO-style letterbox preprocessing, box IoU for NMS, and the
 * platform execution-provider session-options factory.
 */
#pragma once

#include <onnxruntime_cxx_api.h>

#include <cstdint>
#include <string>

namespace onnxcommon {

//! Read an integer / float env var, returning @p fallback when unset or empty.
int EnvInt(const char* name, int fallback);
float EnvFloat(const char* name, float fallback);

/**
 * Aspect-preserving letterbox of interleaved RGB (3 bytes/pixel) into a planar CHW
 * float buffer in [0,1] with Ultralytics gray (114) padding. Returns the scale and
 * pad offsets so detections in model-pixel space map back to the source frame.
 */
void RgbToChwLetterbox(const uint8_t* rgb, int srcW, int srcH, int size, float* out,
                       float& scaleOut, int& padXOut, int& padYOut);

//! Axis-aligned IoU of two boxes given as center + size (any consistent units).
float BoxIoU(float acx, float acy, float aw, float ah,
             float bcx, float bcy, float bw, float bh);

//! Env-var names + log tag so seg and pose keep independent EP toggles.
struct EpConfig
{
    const char* logTag;        //!< e.g. "SegMasker" / "PoseTracker".
    const char* coremlEnv;     //!< e.g. "PROJECTM_SEG_COREML".
    const char* cudaEnv;       //!< e.g. "PROJECTM_SEG_CUDA".
    const char* cudaDeviceEnv; //!< e.g. "PROJECTM_SEG_CUDA_DEVICE".
};

/**
 * Builds SessionOptions with the platform GPU execution provider appended: CoreML
 * (ANE/GPU) on macOS, CUDA on Linux, each gated by @p cfg's env switches. On CUDA
 * success sets useCuda=true and cudaDevice; on any failure it logs and leaves the
 * CPU provider in place.
 */
Ort::SessionOptions MakeSessionOptions(const std::string& modelPath, const EpConfig& cfg,
                                       bool& useCuda, int& cudaDevice);

} // namespace onnxcommon
