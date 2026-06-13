/**
 * @file segMask.hpp
 * @brief ONNX person-segmentation: turns a color frame into an RGBA frame with a
 *        person matte in alpha, computed on the host (CoreML/ANE on macOS).
 *
 * The matte is derived from the *same* color frame, so it is perfectly time-
 * aligned to the RGB and as responsive as the video — unlike a depth camera,
 * whose silhouette trails the color image. The finished RGBA is submitted through
 * the normal Source+refine path; the library snaps the matte edges to the color.
 *
 * Built against an installed onnxruntime when ENABLE_ONNX_SEG is on; otherwise a
 * stub (segMask_stub.cpp) provides the same interface and reports unsupported.
 */
#pragma once

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

class SegMasker
{
public:
    SegMasker();
    ~SegMasker();

    SegMasker(const SegMasker&) = delete;
    SegMasker& operator=(const SegMasker&) = delete;

    /** True when built against onnxruntime (a real backend exists). */
    static bool IsSupported();

    /**
     * Loads a segmentation model. Auto-detects RVM (recurrent) vs a single-input
     * matting model. Input/output tensor names come from the model.
     * @param size Processing size for dynamic-input models (RVM); <=0 = default.
     *        $PROJECTM_SEG_SIZE overrides. Fixed-input models ignore this.
     * @param downsampleRatio RVM internal downsample ratio; <=0 = default (1.0).
     *        $PROJECTM_SEG_DOWNSAMPLE overrides.
     * @return true on success.
     */
    bool Load(const std::string& modelPath, int size = 0, float downsampleRatio = 0.0f);

    bool IsLoaded() const;

    /**
     * Runs segmentation on a BGRA/BGRX color frame and writes an RGBA frame
     * (RGB = the color image, A = person matte) to @p outRGBA (resized to w*h*4).
     * @param mirror Horizontally flip to a selfie-style view.
     */
    void Process(const uint8_t* bgra, int w, int h, bool mirror, std::vector<uint8_t>& outRGBA);

private:
    struct Impl;
    std::unique_ptr<Impl> m_impl;
};
