/**
 * @file depthCapture.hpp
 * @brief Luxonis OAK (depthai) camera backend: RGB + stereo depth, with a
 *        depth-derived foreground mask composited into the alpha channel.
 *
 * Unlike VideoCapture (a plain RGB webcam), this backend uses the stereo depth
 * map to separate a foreground subject from the background. The mask is packed
 * into alpha and the finished RGBA frame is handed to the same submit path the
 * webcam uses; the library's refinement back-end (Source + refine) then cleans
 * up the edges using the RGB as guidance. The host stays signal-agnostic about
 * what the library does with the mask — it only supplies a strong depth prior.
 *
 * Built only when ENABLE_LUXONIS is on and depthai-core is found; otherwise a
 * stub (depthCapture_stub.cpp) provides the same interface and Start() fails.
 */
#pragma once

#include <functional>
#include <memory>

class DepthCapture
{
public:
    /**
     * Parameters controlling how the per-pixel foreground mask is derived from
     * the depth map. All distances are in millimetres. Tuned for a person
     * standing in front of the camera; overridable from config.
     */
    struct MaskParams
    {
        //!< Depth values outside [minDepthMm, maxDepthMm] are treated as invalid.
        float minDepthMm{300.0f};
        float maxDepthMm{6000.0f};
        //!< Foreground is depth within targetDepth +/- bandMm. targetDepth is
        //!< estimated per frame from a near percentile of the central ROI.
        float bandMm{600.0f};
        //!< Soft edge width (mm) for the smoothstep falloff at the band edges.
        float featherMm{200.0f};
        //!< Central region (fraction of frame, 0..1) sampled to estimate the
        //!< foreground subject's depth. 0.5 = central half in each axis.
        float roiFraction{0.5f};
        //!< Robust "nearest subject" percentile (0..1) of valid ROI depths used
        //!< as the foreground target. 0.2 = 20th percentile (closer = smaller).
        float nearPercentile{0.2f};
        //!< Horizontal mirror to match a webcam-style selfie view.
        bool mirror{true};
    };

    /**
     * Frame callback. Invoked from the capture worker thread.
     * @param data Tightly-packed RGBA8 (rows = width * 4 bytes). RGB is the
     *        color image; alpha is the foreground mask (255 = foreground,
     *        0 = background, 128 = unknown/invalid depth -> let color guidance
     *        in the library's fill pass decide).
     * @param width Frame width in pixels.
     * @param height Frame height in pixels.
     */
    using FrameCallback = std::function<void(const void* data, int width, int height)>;

    DepthCapture();
    ~DepthCapture();

    DepthCapture(const DepthCapture&) = delete;
    DepthCapture& operator=(const DepthCapture&) = delete;

    /** True when built against depthai-core (i.e. a real OAK backend exists). */
    static bool IsSupported();

    void SetMaskParams(const MaskParams& params) { m_params = params; }

    /**
     * Opens the first available OAK device and begins streaming.
     * @param callback Per-frame RGBA callback, invoked from a worker thread.
     * @param width,height Requested output resolution (color preview and depth
     *        are produced at this size; depth is aligned to color).
     * @return true if a device was opened and capture started; false otherwise
     *         (no device, depthai not built in, or pipeline error).
     */
    bool Start(FrameCallback callback, int width = 320, int height = 240);

    /** Stops capture and releases the device. */
    void Stop();

    bool IsRunning() const;

private:
    struct Impl;
    std::unique_ptr<Impl> m_impl;
    MaskParams m_params;
};
