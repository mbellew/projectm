/**
 * @file depthCapture_oak.cpp
 * @brief Luxonis OAK (depthai-core v2.x) backend for DepthCapture.
 *
 * Pipeline: full-sensor ColorCamera (12MP, ISP-scaled down to preserve the wide
 * lens FOV) plus a StereoDepth map aligned to the color sensor. A worker thread
 * pairs color + depth, derives a foreground mask from the depth, packs it into
 * alpha, and hands the RGBA frame to the callback. The mask is a coarse,
 * high-confidence prior; the library's refinement back-end snaps it to the
 * color edges.
 *
 * Alignment notes (mirrors ~/scratch/oak/depth_viewer.py, which was tuned on an
 * OAK-D Lite):
 *  - The IMX214's THE_1080_P readout is a center crop with a narrower FOV than
 *    the lens captures. Full-sensor 12MP + setIspScale(1,4) keeps the wide color
 *    FOV so the mono-derived depth covers the visible color frame.
 *  - Even with setDepthAlign(RGB), the mono pair's common-view region is
 *    narrower than the color HFOV (more so at close range, from parallax). The
 *    depth->color sampling applies a per-frame horizontal HFOV stretch so the
 *    mask edges line up with the color image.
 *
 * Compiled only when ENABLE_LUXONIS is on and depthai-core is found.
 */
#include "depthCapture.hpp"

#include <depthai/depthai.hpp>
#include <SDL2/SDL.h>

#include <algorithm>
#include <atomic>
#include <cmath>
#include <cstdint>
#include <thread>
#include <vector>

namespace {

// OAK-D Lite optical geometry (degrees / mm). Used to expand the mono-aligned
// depth out to the color HFOV (see HfovStretch).
constexpr float kColorHfovDeg = 75.0f;
constexpr float kMonoHfovDeg = 71.0f;
constexpr float kBaselineMm = 37.5f; // color-to-each-mono offset (75 mm pair / 2)
constexpr float kFgEmaAlpha = 0.25f; // smoothing on the running foreground depth

// Estimate the foreground subject's depth as the NEAREST substantial depth
// cluster. A band-width window is slid from near to far over a depth histogram;
// the first window holding enough pixel mass wins, and its mass-weighted centroid
// is the target. This is position-independent (the subject can be off-center) and
// tracks "the closest real object" (you) rather than the median — which, when you
// step off-center, would lock onto a mid-distance object or the background and
// invert the mask. The mass floor ignores small speckles / thin intermediate
// objects so it doesn't latch onto them.
float EstimateTargetDepth(const uint16_t* depth, int w, int h,
                          const DepthCapture::MaskParams& p)
{
    constexpr float kBinMm = 100.0f;
    const int minBin = static_cast<int>(p.minDepthMm / kBinMm);
    const int maxBin = static_cast<int>(p.maxDepthMm / kBinMm);
    const int nbins = std::max(1, maxBin - minBin + 1);

    std::vector<int> hist(static_cast<size_t>(nbins), 0);
    int total = 0;
    for (int i = 0; i < w * h; ++i)
    {
        const uint16_t d = depth[i];
        if (d < p.minDepthMm || d > p.maxDepthMm)
        {
            continue;
        }
        const int bin = std::clamp(static_cast<int>(d / kBinMm) - minBin, 0, nbins - 1);
        ++hist[static_cast<size_t>(bin)];
        ++total;
    }
    if (total < 500) // MIN_VALID_PIXELS — not enough depth to trust
    {
        return 0.0f;
    }

    // Sliding window roughly the subject's depth extent; require it to hold a
    // meaningful fraction of the valid pixels (a person-sized blob, not a chair
    // edge or speckle).
    const int winBins = std::max(1, static_cast<int>(p.bandMm / kBinMm));
    const int minMass = std::max(800, static_cast<int>(total * 0.05f));

    for (int start = 0; start <= nbins - 1; ++start)
    {
        const int end = std::min(nbins, start + winBins);
        long mass = 0;
        double weighted = 0.0;
        for (int b = start; b < end; ++b)
        {
            mass += hist[static_cast<size_t>(b)];
            weighted += static_cast<double>(hist[static_cast<size_t>(b)]) *
                        ((minBin + b) * kBinMm + kBinMm * 0.5);
        }
        if (mass >= minMass)
        {
            return static_cast<float>(weighted / static_cast<double>(mass));
        }
    }

    // No window met the mass floor: fall back to the nearest non-empty bin so we
    // still track something rather than going fully unknown.
    for (int b = 0; b < nbins; ++b)
    {
        if (hist[static_cast<size_t>(b)] > 0)
        {
            return (minBin + b) * kBinMm + kBinMm * 0.5f;
        }
    }
    return 0.0f;
}

// Horizontal stretch factor expanding the mono-aligned mask out to the color
// HFOV. At infinity ~tan(color/2)/tan(mono/2); at close range the mono pair's
// common view narrows due to parallax, so more stretch is needed. Mirrors
// hfov_stretch() in the reference.
float HfovStretch(float fgDepthMm)
{
    const float monoHalfTan = std::tan(kMonoHfovDeg * 0.5f * static_cast<float>(M_PI) / 180.0f);
    const float colorHalfTan = std::tan(kColorHfovDeg * 0.5f * static_cast<float>(M_PI) / 180.0f);
    if (fgDepthMm < 200.0f)
    {
        return colorHalfTan / std::max(monoHalfTan - kBaselineMm / 600.0f, 0.1f);
    }
    float effTan = monoHalfTan - kBaselineMm / fgDepthMm;
    effTan = std::max(effTan, 0.1f);
    return colorHalfTan / effTan;
}

// Compose color (BGR888, interleaved) + depth (uint16 mm, aligned) into RGBA8
// with a depth-derived foreground mask in alpha:
//   foreground (within target +/- band) -> 255, fading to 0 over featherMm
//   invalid depth (0 / out of range)    -> 128 (unknown: let color guidance decide)
//   background                          -> 0
// `target` is the (EMA-smoothed) foreground depth; <=0 means "no trustworthy
// depth this frame" so everything becomes unknown and the library's color-guided
// fill takes over.
void ComposeRGBA(const uint8_t* bgr, const uint16_t* depth, int w, int h,
                 const DepthCapture::MaskParams& p, float target, uint8_t* outRGBA)
{
    const bool haveTarget = target > 0.0f;
    const float band = p.bandMm;
    const float feather = std::max(1.0f, p.featherMm);
    // Map an output column to a depth column: undo the horizontal HFOV stretch
    // about the frame center. depthX = (x - w/2) / stretch + w/2.
    const float invStretch = haveTarget ? 1.0f / HfovStretch(target) : 1.0f;
    const float halfW = w * 0.5f;

    for (int y = 0; y < h; ++y)
    {
        const uint16_t* depthRow = depth + static_cast<size_t>(y) * w;
        for (int x = 0; x < w; ++x)
        {
            const int colX = p.mirror ? (w - 1 - x) : x;
            const uint8_t* c = bgr + (static_cast<size_t>(y) * w + colX) * 3;
            uint8_t* o = outRGBA + (static_cast<size_t>(y) * w + x) * 4;
            o[0] = c[2]; // R
            o[1] = c[1]; // G
            o[2] = c[0]; // B

            uint8_t alpha = 128; // default: unknown
            if (haveTarget)
            {
                // Sample depth at the HFOV-corrected, mirror-matched column.
                const int dCol = p.mirror ? (w - 1 - x) : x;
                int depthX = static_cast<int>(std::lround((dCol - halfW) * invStretch + halfW));
                depthX = std::clamp(depthX, 0, w - 1);
                const uint16_t d = depthRow[depthX];
                if (d >= p.minDepthMm && d <= p.maxDepthMm)
                {
                    const float dist = std::fabs(static_cast<float>(d) - target);
                    const float t = (dist - band) / feather; // <0 inside band
                    const float fg = 1.0f - std::clamp(t, 0.0f, 1.0f);
                    alpha = static_cast<uint8_t>(std::lround(fg * 255.0f));
                }
                // else: invalid depth -> leave alpha at 128 (unknown)
            }
            o[3] = alpha;
        }
    }
}

} // namespace

struct DepthCapture::Impl
{
    std::unique_ptr<dai::Device> device;
    std::thread worker;
    std::atomic<bool> running{false};
};

DepthCapture::DepthCapture()
    : m_impl(std::make_unique<Impl>())
{
}

DepthCapture::~DepthCapture()
{
    Stop();
}

bool DepthCapture::IsSupported()
{
    return true;
}

bool DepthCapture::Start(FrameCallback callback, int width, int height)
{
    if (m_impl->running)
    {
        return false;
    }

    try
    {
        dai::Pipeline pipeline;

        auto camRgb = pipeline.create<dai::node::ColorCamera>();
        camRgb->setBoardSocket(dai::CameraBoardSocket::CAM_A); // RGB
        // Full-sensor mode + ISP downscale preserves the wide lens FOV (the
        // THE_1080_P readout is a narrow center crop); preview is the host frame.
        camRgb->setResolution(dai::ColorCameraProperties::SensorResolution::THE_12_MP);
        camRgb->setIspScale(1, 4);
        camRgb->setPreviewSize(width, height);
        camRgb->setPreviewKeepAspectRatio(false);
        camRgb->setInterleaved(true);
        camRgb->setColorOrder(dai::ColorCameraProperties::ColorOrder::BGR);
        camRgb->setFps(30.0f);

        auto monoL = pipeline.create<dai::node::MonoCamera>();
        monoL->setResolution(dai::MonoCameraProperties::SensorResolution::THE_400_P);
        monoL->setBoardSocket(dai::CameraBoardSocket::CAM_B); // LEFT
        monoL->setFps(30.0f);

        auto monoR = pipeline.create<dai::node::MonoCamera>();
        monoR->setResolution(dai::MonoCameraProperties::SensorResolution::THE_400_P);
        monoR->setBoardSocket(dai::CameraBoardSocket::CAM_C); // RIGHT
        monoR->setFps(30.0f);

        auto stereo = pipeline.create<dai::node::StereoDepth>();
        stereo->setDefaultProfilePreset(dai::node::StereoDepth::PresetMode::HIGH_DENSITY);
        stereo->setDepthAlign(dai::CameraBoardSocket::CAM_A); // reproject depth into color (RGB)
        stereo->setOutputSize(width, height);
        // Left/right check adds a second matching pass (latency) for cleaner
        // occlusion edges. Off here to minimize the depth-vs-color latency skew
        // that makes the mask trail motion; the library fill covers the holes.
        stereo->setLeftRightCheck(false);
        stereo->setSubpixel(false);
        stereo->setExtendedDisparity(false);
        // Post-processing: kill speckles, fill small holes, stabilize over time
        // so the prior isn't noisy. Values from the reference.
        auto cfg = stereo->initialConfig.get();
        cfg.postProcessing.speckleFilter.enable = true;
        cfg.postProcessing.speckleFilter.speckleRange = 50;
        // Temporal filter is a cross-frame EMA on depth: it stabilizes but adds
        // latency/ghosting. Off for responsiveness — the mask band + library
        // refine tolerate the extra per-frame noise.
        cfg.postProcessing.temporalFilter.enable = false;
        // Spatial filter is iterative on-device (latency). Off for responsiveness;
        // the library's color-guided fill handles interior holes instead.
        cfg.postProcessing.spatialFilter.enable = false;
        cfg.postProcessing.spatialFilter.holeFillingRadius = 2;
        cfg.postProcessing.spatialFilter.numIterations = 1;
        cfg.postProcessing.thresholdFilter.minRange = static_cast<int>(m_params.minDepthMm);
        cfg.postProcessing.thresholdFilter.maxRange = static_cast<int>(m_params.maxDepthMm);
        cfg.postProcessing.decimationFilter.decimationFactor = 1;
        stereo->initialConfig.set(cfg);

        monoL->out.link(stereo->left);
        monoR->out.link(stereo->right);

        auto xoutRgb = pipeline.create<dai::node::XLinkOut>();
        xoutRgb->setStreamName("rgb");
        camRgb->preview.link(xoutRgb->input);

        auto xoutDepth = pipeline.create<dai::node::XLinkOut>();
        xoutDepth->setStreamName("depth");
        stereo->depth.link(xoutDepth->input);

        m_impl->device = std::make_unique<dai::Device>(pipeline);
    }
    catch (const std::exception& e)
    {
        SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION, "[DepthCapture] OAK pipeline failed: %s", e.what());
        m_impl->device.reset();
        return false;
    }

    m_impl->running = true;
    const MaskParams params = m_params;
    m_impl->worker = std::thread([this, callback, params, width, height]() {
        // Depth-1, non-blocking: each get() returns the freshest frame and old
        // frames are dropped rather than buffered (minimizes capture latency).
        auto qRgb = m_impl->device->getOutputQueue("rgb", 1, false);
        auto qDepth = m_impl->device->getOutputQueue("depth", 1, false);
        std::vector<uint8_t> rgba(static_cast<size_t>(width) * height * 4);
        float fgEma = -1.0f; // running (EMA-smoothed) foreground depth

        while (m_impl->running)
        {
            // Blocking get; on shutdown the queue is closed and get() returns null.
            auto colorFrame = qRgb->get<dai::ImgFrame>();
            auto depthFrame = qDepth->get<dai::ImgFrame>();
            if (!m_impl->running || !colorFrame || !depthFrame)
            {
                continue;
            }

            if (static_cast<int>(colorFrame->getWidth()) != width ||
                static_cast<int>(colorFrame->getHeight()) != height ||
                static_cast<int>(depthFrame->getWidth()) != width ||
                static_cast<int>(depthFrame->getHeight()) != height)
            {
                continue; // warmup / size-mismatch frame
            }

            const uint8_t* bgr = colorFrame->getData().data();
            const uint16_t* depth = reinterpret_cast<const uint16_t*>(depthFrame->getData().data());

            const float sample = EstimateTargetDepth(depth, width, height, params);
            if (sample > 0.0f)
            {
                fgEma = (fgEma < 0.0f) ? sample : kFgEmaAlpha * sample + (1.0f - kFgEmaAlpha) * fgEma;
            }
            ComposeRGBA(bgr, depth, width, height, params, fgEma > 0.0f ? fgEma : 0.0f, rgba.data());
            callback(rgba.data(), width, height);
        }
    });

    SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, "[DepthCapture] OAK capture started (%dx%d).", width, height);
    return true;
}

void DepthCapture::Stop()
{
    if (!m_impl->running)
    {
        return;
    }
    m_impl->running = false;
    if (m_impl->device)
    {
        m_impl->device->close(); // unblock the worker's queue get()
    }
    if (m_impl->worker.joinable())
    {
        m_impl->worker.join();
    }
    m_impl->device.reset();
}

bool DepthCapture::IsRunning() const
{
    return m_impl->running;
}
