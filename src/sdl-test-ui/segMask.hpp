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

/**
 * Per-stage wall-clock cost of the last Process() call, in milliseconds. SEG_MASK_PERF.md's whole
 * diagnosis (that the CPU work around the model dominates, not the model) is an ESTIMATE read off
 * the code -- these numbers are how we confirm or refute it before acting on it.
 *
 * Fields not exercised by the active model family stay 0 (e.g. compositeMs on the YOLO path).
 */
struct SegTimings
{
    double rgbMs{0.0};       //!< BGRA -> interleaved RGB, at full camera res.
    double inferMs{0.0};     //!< Preprocess (downscale/normalize) + the ONNX Run itself.
    double compositeMs{0.0}; //!< Matte upscale + RGBA composite, at full camera res.
    double hardenMs{0.0};    //!< HardenAlpha, at full camera res (0 when disabled).
    double depthMs{0.0};     //!< ApplyDepthGate: an entire second model + connected components.
    double totalMs{0.0};     //!< Whole Process() call.
};

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

    /**
     * Loads a second model whose matte is multiplied into the primary's each frame
     * (e.g. RVM soft matte x a person-class mask = a soft, people-only matte).
     * Same auto-detection as Load(). Call after Load(). @return true on success.
     */
    bool LoadSecondary(const std::string& modelPath, int size = 0, float downsampleRatio = 0.0f,
                       const std::string& combine = "multiply", float gateThreshold = 0.5f);

    /**
     * Loads a monocular relative-depth model (Depth Anything V2) used to drop
     * background people from the matte. After the person matte is computed each
     * frame, the matte is split into connected components, each component's median
     * relative depth is measured, and components sitting far behind the nearest one
     * are removed -- so spectators and passers-by are cut while everyone up front is
     * kept. Relative depth is sufficient: components are only ranked, never measured.
     * Call after Load(). No-op family detection; the model is a single RGB input
     * (ImageNet-normalized) with a single-channel depth output (larger = closer).
     * @param size   Depth processing long-side in px (snapped to a multiple of 14);
     *               <=0 = default. $PROJECTM_SEG_DEPTH_SIZE overrides.
     * @param band   Keep components within this normalized closeness (0..1) of the
     *               nearest; larger = keep more people behind the front. <=0 = default.
     * @param invert Set true if the model outputs larger = farther. Default false.
     * @return true on success.
     */
    bool LoadDepth(const std::string& modelPath, int size = 0, float band = 0.0f,
                   bool invert = false);

    /**
     * Sets the matte-hardening smoothstep edges (see HardenAlpha). lo<=0 and hi>=1
     * disables it (raw matte). $PROJECTM_SEG_HARDEN_LO / _HI override these.
     */
    void SetHarden(float lo, float hi);

    bool IsLoaded() const;

    /** True when a depth model is loaded and a depth map from the last frame is available. */
    bool HasDepth() const;

    /**
     * Samples the last frame's relative-depth map as normalized closeness at a point.
     * @param fx,fy Normalized position, [0,1], fx left->right and fy BOTTOM->top (matches the
     *        seg_ and touch_ variable convention).
     * @return Closeness in [0,1] (1 = nearest), or -1 if no depth map is available. Bilinear.
     * Safe to call right after Process() on the same thread (reads the retained depth buffer).
     */
    float SampleDepth(float fx, float fy) const;

    /**
     * Runs segmentation on a BGRA/BGRX color frame and writes an RGBA frame
     * (RGB = the color image, A = person matte) to @p outRGBA (resized to w*h*4).
     *
     * NOTE: the depth gate's verdict is NOT baked into the alpha. It is left as a coarse weight
     * grid (see GateGrid) for the GPU to multiply in, because doing it here costs a full-resolution
     * pass per frame on the capture thread. Callers that consume the alpha on the CPU must weight
     * it themselves with SampleGate, or they will still see the people the gate removed.
     *
     * @param mirror Horizontally flip to a selfie-style view.
     */
    void Process(const uint8_t* bgra, int w, int h, bool mirror, std::vector<uint8_t>& outRGBA);

    /** True when the last Process() produced a depth-gate weight grid. */
    bool HasGate() const;

    /**
     * Centroid of the depth gate's ANCHOR component -- the figure whose depth sets the keep band,
     * i.e. the one the gate treats as the primary subject. Note this is only ONE of the system's
     * three notions of "the subject" (the others being pose's `poses.front()` and the matte
     * centroid behind seg_cx/seg_cy); they are elected independently and can disagree.
     * @param fx,fy Normalized, [0,1], fx left->right and fy BOTTOM->top (as SampleDepth/SampleGate).
     * @return false if no anchor has been elected yet (no gate, or only noise-sized blobs).
     */
    bool AnchorCentroid(float& fx, float& fy) const;

    /**
     * The depth gate's per-cell keep weights from the last Process(): [0,1], 1 = keep, row-major,
     * row 0 = top of the frame, in un-mirrored camera space. Hand this to the library
     * (projectm_video_submit_alpha_gate) to have it multiplied into the matte on the GPU.
     * @return The grid, or nullptr (and 0 dims) if there is no gate. Valid until the next Process().
     */
    const float* GateGrid(int& gridW, int& gridH) const;

    /**
     * Samples the gate's keep weight at a point, for CPU consumers of the matte alpha.
     * @param fx,fy Normalized, [0,1], fx left->right and fy BOTTOM->top (as SampleDepth).
     * @return Weight in [0,1]; 1 (keep) when there is no gate. Bilinear, matching the GPU apply.
     */
    float SampleGate(float fx, float fy) const;

    /** Per-stage cost of the last Process() call. See SegTimings. */
    const SegTimings& LastTimings() const;

    /**
     * The interleaved RGB frame Process() built from the BGRA input (w*h*3), in the orientation
     * Process() was given (i.e. mirrored only if it was asked to mirror).
     *
     * Exposed so the pose tracker can reuse it instead of converting the very same BGRA frame to
     * the very same RGB a second time at full camera resolution, every frame -- which is what it
     * used to do. Valid until the next Process() call, on the same thread.
     *
     * @return The buffer, or nullptr if Process() has not run yet.
     */
    const uint8_t* RgbFrame() const;

private:
    /**
     * Runs the loaded depth model on the last RGB frame and zeroes the alpha of
     * matte components that sit far behind the nearest one (see LoadDepth). No-op
     * when no depth model is loaded.
     */
    void ApplyDepthGate(int w, int h, std::vector<uint8_t>& outRGBA);

    /**
     * Contrast-remaps the matte alpha through smoothstep(lo, hi, a) to harden a soft
     * matte (e.g. RVM): alpha <= lo -> 0, alpha >= hi -> 1, smooth between -- reducing
     * partial-alpha ghosting. Controlled by $PROJECTM_SEG_HARDEN_LO / _HI; a no-op when
     * lo<=0 and hi>=1 (the default), preserving the raw matte.
     */
    void HardenAlpha(int w, int h, std::vector<uint8_t>& outRGBA);

    struct Impl;
    std::unique_ptr<Impl> m_impl;
};
