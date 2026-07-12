/**
 * @file segMask.cpp
 * @brief ONNX Runtime person-segmentation backend (CoreML EP on macOS).
 *
 * Supports two model families, auto-detected at load:
 *  - Single-input matting (e.g. MODNet): one image in -> one matte out, fixed
 *    input size, normalized to [-1,1].
 *  - Robust Video Matting (RVM): src + recurrent state (r1i..r4i) +
 *    downsample_ratio -> pha + recurrent state (r1o..r4o). Dynamic input, run at
 *    a small size for real-time speed; normalized to [0,1]. The recurrent state
 *    is carried frame to frame, giving temporally stable, fast mattes.
 *
 * Per frame: build the RGB output frame, bilinearly downscale to the model input,
 * normalize, run on the ANE/GPU, bilinearly upscale the matte into the alpha. The
 * matte is in the output frame's space, so it aligns with the RGB.
 *
 * Compiled only when ENABLE_ONNX_SEG is on and onnxruntime is found.
 */
#include "segMask.hpp"

#include "onnxCommon.hpp"

#include <onnxruntime_cxx_api.h>
#include <SDL2/SDL.h>

#include <algorithm>
#include <array>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstdlib>

namespace {

using onnxcommon::EnvFloat;
using onnxcommon::EnvInt;

// Bilinear downscale of interleaved RGB (3 bytes/pixel) into a planar CHW float
// buffer, applying out = byte * scale + bias (per-model normalization).
void RgbToChw(const uint8_t* rgb, int srcW, int srcH, int dstW, int dstH,
              float scale, float bias, float* out)
{
    const float sx = static_cast<float>(srcW) / dstW;
    const float sy = static_cast<float>(srcH) / dstH;
    const int plane = dstW * dstH;
    for (int y = 0; y < dstH; ++y)
    {
        const float fy = (y + 0.5f) * sy - 0.5f;
        const int y0 = std::clamp(static_cast<int>(std::floor(fy)), 0, srcH - 1);
        const int y1 = std::min(y0 + 1, srcH - 1);
        const float wy = std::clamp(fy - y0, 0.0f, 1.0f);
        for (int x = 0; x < dstW; ++x)
        {
            const float fx = (x + 0.5f) * sx - 0.5f;
            const int x0 = std::clamp(static_cast<int>(std::floor(fx)), 0, srcW - 1);
            const int x1 = std::min(x0 + 1, srcW - 1);
            const float wx = std::clamp(fx - x0, 0.0f, 1.0f);

            const uint8_t* p00 = rgb + (static_cast<size_t>(y0) * srcW + x0) * 3;
            const uint8_t* p01 = rgb + (static_cast<size_t>(y0) * srcW + x1) * 3;
            const uint8_t* p10 = rgb + (static_cast<size_t>(y1) * srcW + x0) * 3;
            const uint8_t* p11 = rgb + (static_cast<size_t>(y1) * srcW + x1) * 3;
            const int dstIdx = y * dstW + x;
            for (int c = 0; c < 3; ++c)
            {
                const float top = p00[c] * (1 - wx) + p01[c] * wx;
                const float bot = p10[c] * (1 - wx) + p11[c] * wx;
                out[c * plane + dstIdx] = (top * (1 - wy) + bot * wy) * scale + bias;
            }
        }
    }
}

// Bilinear downscale of interleaved RGB into a planar CHW float buffer using U²-Net's
// normalization: divide by the per-image max (a single scalar over the resized RGB), then
// per-channel ImageNet mean/std. Matches the rembg/U²-Net preprocessing the model trained on.
void RgbToChwImageNet(const uint8_t* rgb, int srcW, int srcH, int dstW, int dstH, float* out)
{
    const float sx = static_cast<float>(srcW) / dstW;
    const float sy = static_cast<float>(srcH) / dstH;
    const int plane = dstW * dstH;

    // Pass 1: bilinear resample raw [0,255] values, tracking the global (cross-channel) max.
    float maxVal = 1e-6f;
    for (int y = 0; y < dstH; ++y)
    {
        const float fy = (y + 0.5f) * sy - 0.5f;
        const int y0 = std::clamp(static_cast<int>(std::floor(fy)), 0, srcH - 1);
        const int y1 = std::min(y0 + 1, srcH - 1);
        const float wy = std::clamp(fy - y0, 0.0f, 1.0f);
        for (int x = 0; x < dstW; ++x)
        {
            const float fx = (x + 0.5f) * sx - 0.5f;
            const int x0 = std::clamp(static_cast<int>(std::floor(fx)), 0, srcW - 1);
            const int x1 = std::min(x0 + 1, srcW - 1);
            const float wx = std::clamp(fx - x0, 0.0f, 1.0f);

            const uint8_t* p00 = rgb + (static_cast<size_t>(y0) * srcW + x0) * 3;
            const uint8_t* p01 = rgb + (static_cast<size_t>(y0) * srcW + x1) * 3;
            const uint8_t* p10 = rgb + (static_cast<size_t>(y1) * srcW + x0) * 3;
            const uint8_t* p11 = rgb + (static_cast<size_t>(y1) * srcW + x1) * 3;
            const int dstIdx = y * dstW + x;
            for (int c = 0; c < 3; ++c)
            {
                const float top = p00[c] * (1 - wx) + p01[c] * wx;
                const float bot = p10[c] * (1 - wx) + p11[c] * wx;
                const float v = top * (1 - wy) + bot * wy;
                out[c * plane + dstIdx] = v;
                maxVal = std::max(maxVal, v);
            }
        }
    }

    // Pass 2: out = (v / maxVal - mean[c]) / std[c].
    static const float mean[3] = {0.485f, 0.456f, 0.406f};
    static const float stdv[3] = {0.229f, 0.224f, 0.225f};
    const float invMax = 1.0f / maxVal;
    for (int c = 0; c < 3; ++c)
    {
        const float m = mean[c];
        const float invStd = 1.0f / stdv[c];
        float* ch = out + c * plane;
        for (int i = 0; i < plane; ++i)
        {
            ch[i] = (ch[i] * invMax - m) * invStd;
        }
    }
}

// Bilinear resize of interleaved RGB into a planar CHW float buffer using standard ImageNet
// preprocessing: scale bytes to [0,1] (divide by 255), then per-channel (x - mean) / std. This
// is what Depth Anything V2 (and most torchvision/transformers vision models) expect -- distinct
// from RgbToChwImageNet above, which divides by the per-image max (U²-Net's quirk).
void RgbToChwImageNetStd(const uint8_t* rgb, int srcW, int srcH, int dstW, int dstH, float* out)
{
    static const float mean[3] = {0.485f, 0.456f, 0.406f};
    static const float stdv[3] = {0.229f, 0.224f, 0.225f};
    const float sx = static_cast<float>(srcW) / dstW;
    const float sy = static_cast<float>(srcH) / dstH;
    const int plane = dstW * dstH;
    for (int y = 0; y < dstH; ++y)
    {
        const float fy = (y + 0.5f) * sy - 0.5f;
        const int y0 = std::clamp(static_cast<int>(std::floor(fy)), 0, srcH - 1);
        const int y1 = std::min(y0 + 1, srcH - 1);
        const float wy = std::clamp(fy - y0, 0.0f, 1.0f);
        for (int x = 0; x < dstW; ++x)
        {
            const float fx = (x + 0.5f) * sx - 0.5f;
            const int x0 = std::clamp(static_cast<int>(std::floor(fx)), 0, srcW - 1);
            const int x1 = std::min(x0 + 1, srcW - 1);
            const float wx = std::clamp(fx - x0, 0.0f, 1.0f);

            const uint8_t* p00 = rgb + (static_cast<size_t>(y0) * srcW + x0) * 3;
            const uint8_t* p01 = rgb + (static_cast<size_t>(y0) * srcW + x1) * 3;
            const uint8_t* p10 = rgb + (static_cast<size_t>(y1) * srcW + x0) * 3;
            const uint8_t* p11 = rgb + (static_cast<size_t>(y1) * srcW + x1) * 3;
            const int dstIdx = y * dstW + x;
            for (int c = 0; c < 3; ++c)
            {
                const float top = p00[c] * (1 - wx) + p01[c] * wx;
                const float bot = p10[c] * (1 - wx) + p11[c] * wx;
                const float v = (top * (1 - wy) + bot * wy) / 255.0f;
                out[c * plane + dstIdx] = (v - mean[c]) / stdv[c];
            }
        }
    }
}

float SampleMatte(const float* matte, int mw, int mh, float fx, float fy)
{
    const int x0 = std::clamp(static_cast<int>(std::floor(fx)), 0, mw - 1);
    const int x1 = std::min(x0 + 1, mw - 1);
    const int y0 = std::clamp(static_cast<int>(std::floor(fy)), 0, mh - 1);
    const int y1 = std::min(y0 + 1, mh - 1);
    const float wx = std::clamp(fx - x0, 0.0f, 1.0f);
    const float wy = std::clamp(fy - y0, 0.0f, 1.0f);
    const float top = matte[y0 * mw + x0] * (1 - wx) + matte[y0 * mw + x1] * wx;
    const float bot = matte[y1 * mw + x0] * (1 - wx) + matte[y1 * mw + x1] * wx;
    return top * (1 - wy) + bot * wy;
}

// --- YOLOv8/v11-seg helpers (person-class instance masks) ---

float Sigmoid(float x) { return 1.0f / (1.0f + std::exp(-x)); }

// Person detection carrying its instance-mask coefficient offset (the box math itself lives in
// onnxcommon::BoxIoU). RgbToChwLetterbox / MakeSessionOptions are also shared via onnxCommon.
struct YoloDet
{
    float cx, cy, w, h, score;
    int coeffOffset; // start index of this det's mask coefficients in the flat coeff store
};

} // namespace

struct SegMasker::Impl
{
    Ort::Env env{ORT_LOGGING_LEVEL_WARNING, "projectm-seg"};
    std::unique_ptr<Ort::Session> session;
    Ort::AllocatorWithDefaultOptions alloc;

    bool rvm{false};
    bool u2net{false};                  // U²-Net human seg: ImageNet-normalized input, min/max-normalized single-mask output.
    bool yolo{false};                   // YOLOv8/v11-seg: letterboxed input, detection + proto outputs, person-class union.
    std::vector<float> yoloMask;        // Scratch: person mask at proto resolution.

    std::unique_ptr<SegMasker> secondary;   // Optional 2nd model; its matte combines into the primary's.
    std::vector<uint8_t> secondaryRGBA;     // Scratch for the secondary model's output.
    bool secondaryGate{false};              // false = multiply; true = soft gate (reject below threshold only).
    float gateThresh{0.5f};                 // Gate center (PROJECTM_SEG_GATE).
    std::vector<std::string> inNames;   // model input order
    std::vector<std::string> outNames;  // model output order
    int inW{512};
    int inH{512};
    float normScale{1.0f / 127.5f};     // MODNet default
    float normBias{-1.0f};
    float downsampleRatio{1.0f};

    // RVM recurrent state (r1i..r4i), carried frame to frame. When running on the CUDA EP these
    // are kept device-resident (see binding) so they never round-trip to host between frames.
    std::vector<Ort::Value> recurrent;
    float ratioVal{1.0f};

    std::vector<float> inputBuf;        // src CHW
    std::vector<uint8_t> rgbBuf;        // interleaved RGB output frame

    // --- CUDA execution-provider I/O binding (see Load) -----------------------------------------
    // When the CUDA EP is active we drive Run() through an IoBinding instead of plain input/output
    // arrays. This lets RVM's recurrent hidden state stay on the GPU frame-to-frame (it is computed
    // and consumed on-device and never read by the CPU), and lets the matte land in pinned host
    // memory for a faster device->host copy. Off (false) on CPU/CoreML — the host path is unchanged.
    bool useCuda{false};
    int cudaDevice{0};
    std::unique_ptr<Ort::IoBinding> binding;

    // --- Monocular depth gate (see LoadDepth / ApplyDepthGate) ----------------------------------
    // Optional Depth Anything V2 session. When present, after the matte is built we run depth on
    // the same frame, split the matte into connected components, take each component's median
    // relative depth, and zero the alpha of components sitting far behind the nearest one --
    // removing background spectators while keeping everyone up front. Its own session (shares the
    // GPU EP via MakeSessionOptions); no IoBinding, no recurrent state -- a plain single-shot run.
    std::unique_ptr<Ort::Session> depthSession;
    std::vector<std::string> depthInNames;
    std::vector<std::string> depthOutNames;
    int depthSize{392};                 // processing long-side (px); snapped to a multiple of 14
    int depthW{0};                      // per-frame input dims (aspect-matched, multiples of 14)
    int depthH{0};
    float depthBand{0.20f};             // keep components within this normalized closeness of the nearest
    bool depthInvert{false};            // false: larger model output = closer (Depth Anything default)
    std::vector<float> depthInput;      // CHW input scratch (pixel_values)
    std::vector<float> depthMap;        // HxW relative depth (copied out of the model)
    int depthMapW{0};
    int depthMapH{0};
    std::vector<int> ccLabel;           // connected-component label per grid cell (-1 = background)
    std::vector<float> cellWeight;      // per-grid-cell keep weight (1 = keep, 0 = drop)
    int gateW{0};                       // cellWeight grid dims; the grid is handed to the library,
    int gateH{0};                       // which multiplies it into the matte on the GPU.

    SegTimings timings{}; //!< Per-stage cost of the last Process() (see SEG_MASK_PERF.md).

    // Matte-hardening smoothstep edges (see HardenAlpha). lo<=0 && hi>=1 => disabled (raw matte).
    float hardenLo{0.0f};
    float hardenHi{1.0f};
};

SegMasker::SegMasker()
    : m_impl(std::make_unique<Impl>())
{
}

SegMasker::~SegMasker() = default;

bool SegMasker::IsSupported()
{
    return true;
}

bool SegMasker::Load(const std::string& modelPath, int size, float downsampleRatio)
{
    try
    {
        const onnxcommon::EpConfig ep{"SegMasker", "PROJECTM_SEG_COREML", "PROJECTM_SEG_CUDA",
                                      "PROJECTM_SEG_CUDA_DEVICE"};
        Ort::SessionOptions options =
            onnxcommon::MakeSessionOptions(modelPath, ep, m_impl->useCuda, m_impl->cudaDevice);

        m_impl->session = std::make_unique<Ort::Session>(m_impl->env, modelPath.c_str(), options);

        // IoBinding is only used when the CUDA EP actually loaded. If session creation succeeded but
        // CUDA didn't (we fell back to CPU), keep useCuda false so the plain host Run path is used.
        if (m_impl->useCuda)
        {
            m_impl->binding = std::make_unique<Ort::IoBinding>(*m_impl->session);
        }

        const size_t nIn = m_impl->session->GetInputCount();
        const size_t nOut = m_impl->session->GetOutputCount();
        m_impl->inNames.clear();
        m_impl->outNames.clear();
        for (size_t i = 0; i < nIn; ++i)
        {
            m_impl->inNames.emplace_back(m_impl->session->GetInputNameAllocated(i, m_impl->alloc).get());
        }
        for (size_t i = 0; i < nOut; ++i)
        {
            m_impl->outNames.emplace_back(m_impl->session->GetOutputNameAllocated(i, m_impl->alloc).get());
        }

        // RVM is recognized by its downsample_ratio control input.
        m_impl->rvm = std::any_of(m_impl->inNames.begin(), m_impl->inNames.end(),
                                  [](const std::string& n) { return n == "downsample_ratio"; });

        // U²-Net human-seg shares the single-input shape of MODNet but needs ImageNet
        // normalization and min/max output normalization, so detect it by filename.
        m_impl->u2net = !m_impl->rvm &&
                        (modelPath.find("u2net") != std::string::npos ||
                         modelPath.find("U2Net") != std::string::npos ||
                         modelPath.find("u2netp") != std::string::npos);

        // YOLOv8/v11-seg: detection + mask-prototype outputs; we keep the person class only.
        m_impl->yolo = !m_impl->rvm &&
                       (modelPath.find("yolo") != std::string::npos ||
                        modelPath.find("YOLO") != std::string::npos);

        // Processing size: $PROJECTM_SEG_SIZE (raw px) > caller (config) > default.
        const int defSize = m_impl->rvm ? 256 : 512;
        const int cfgSize = (size > 0) ? size : defSize;
        const int segSize = std::max(64, EnvInt("PROJECTM_SEG_SIZE", cfgSize));

        if (m_impl->rvm)
        {
            m_impl->normScale = 1.0f / 255.0f; // RVM expects [0,1]
            m_impl->normBias = 0.0f;
            m_impl->inW = m_impl->inH = segSize;
            m_impl->downsampleRatio =
                EnvFloat("PROJECTM_SEG_DOWNSAMPLE", downsampleRatio > 0.0f ? downsampleRatio : 1.0f);
            // Zero-initialize the recurrent state; RVM grows it internally and we
            // feed each frame's r*o back as the next r*i.
            m_impl->recurrent.clear();
            const std::array<int64_t, 4> zeroShape{1, 1, 1, 1};
            for (int k = 0; k < 4; ++k)
            {
                auto t = Ort::Value::CreateTensor<float>(m_impl->alloc, zeroShape.data(), zeroShape.size());
                *t.GetTensorMutableData<float>() = 0.0f;
                m_impl->recurrent.push_back(std::move(t));
            }
        }
        else if (m_impl->yolo)
        {
            // Letterbox preprocessing does its own /255; normScale/normBias are unused. Input is a
            // square (default 640); honor a fixed export, else snap the requested size to /32.
            const auto shape = m_impl->session->GetInputTypeInfo(0)
                                   .GetTensorTypeAndShapeInfo().GetShape();
            if (shape.size() == 4 && shape[2] > 0 && shape[3] > 0)
            {
                m_impl->inH = static_cast<int>(shape[2]);
                m_impl->inW = static_cast<int>(shape[3]);
            }
            else
            {
                const int s = std::max(64, EnvInt("PROJECTM_SEG_SIZE", 640));
                m_impl->inW = m_impl->inH = std::max(32, (s / 32) * 32);
            }
        }
        else if (m_impl->u2net)
        {
            // Input normalization is handled by RgbToChwImageNet (per-channel ImageNet
            // mean/std); normScale/normBias are unused for this family.
            const auto shape = m_impl->session->GetInputTypeInfo(0)
                                   .GetTensorTypeAndShapeInfo().GetShape();
            const bool fixed = (shape.size() == 4 && shape[2] > 0 && shape[3] > 0);
            // Opt-in override to experiment with higher input resolution (e.g. full-res).
            // Only works if the ONNX graph has resizable input axes; a fixed-shape export
            // will make Run() throw (caught in Process -> passthrough).
            const int override = EnvInt("PROJECTM_SEG_SIZE", 0);
            if (override > 0)
            {
                m_impl->inW = m_impl->inH = override;
            }
            else if (fixed)
            {
                m_impl->inH = static_cast<int>(shape[2]);
                m_impl->inW = static_cast<int>(shape[3]);
            }
            else
            {
                m_impl->inW = m_impl->inH = 320; // U²-Net default input size
            }
            SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                        "[SegMasker] u2net declared input %s (%lldx%lld); using %dx%d.",
                        fixed ? "FIXED" : "dynamic",
                        fixed ? static_cast<long long>(shape[3]) : -1LL,
                        fixed ? static_cast<long long>(shape[2]) : -1LL,
                        m_impl->inW, m_impl->inH);
        }
        else
        {
            // MODNet (and other single-input matting models): input normalized to [-1,1], and a
            // single [1,1,H,W] alpha output already in [0,1] (used directly -- no min/max stretch).
            m_impl->normScale = 1.0f / 127.5f;
            m_impl->normBias = -1.0f;
            const auto shape = m_impl->session->GetInputTypeInfo(0)
                                   .GetTensorTypeAndShapeInfo().GetShape();
            if (shape.size() == 4 && shape[2] > 0 && shape[3] > 0)
            {
                m_impl->inH = static_cast<int>(shape[2]);
                m_impl->inW = static_cast<int>(shape[3]);
            }
            else
            {
                // Dynamic axes: MODNet requires the input H/W to be multiples of 32, so snap down.
                m_impl->inW = m_impl->inH = std::max(32, (segSize / 32) * 32);
            }
        }

        m_impl->inputBuf.resize(static_cast<size_t>(3) * m_impl->inW * m_impl->inH);
        SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                    "[SegMasker] Loaded '%s' (%s, input %dx%d, ratio %.2f).",
                    modelPath.c_str(),
                    m_impl->rvm ? "RVM" : (m_impl->yolo ? "YOLO-seg (person)" : (m_impl->u2net ? "U2Net-human" : "MODNet/matte")),
                    m_impl->inW, m_impl->inH, m_impl->downsampleRatio);
        return true;
    }
    catch (const std::exception& e)
    {
        SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION, "[SegMasker] Failed to load model: %s", e.what());
        m_impl->session.reset();
        return false;
    }
}

bool SegMasker::LoadSecondary(const std::string& modelPath, int size, float downsampleRatio,
                              const std::string& combine, float gateThreshold)
{
    m_impl->secondary = std::make_unique<SegMasker>();
    if (!m_impl->secondary->Load(modelPath, size, downsampleRatio))
    {
        m_impl->secondary.reset();
        SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION,
                    "[SegMasker] Secondary model '%s' failed to load; using primary only.",
                    modelPath.c_str());
        return false;
    }
    m_impl->secondaryGate = (combine == "gate");
    m_impl->gateThresh = gateThreshold;
    SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                "[SegMasker] Secondary model loaded; combine=%s.",
                m_impl->secondaryGate ? "gate" : "multiply");
    return true;
}

bool SegMasker::LoadDepth(const std::string& modelPath, int size, float band, bool invert)
{
    try
    {
        bool dCuda = false; // depth runs single-shot; it doesn't need IoBinding/recurrent state.
        int dDev = 0;
        const onnxcommon::EpConfig ep{"SegMasker", "PROJECTM_SEG_COREML", "PROJECTM_SEG_CUDA",
                                      "PROJECTM_SEG_CUDA_DEVICE"};
        Ort::SessionOptions options = onnxcommon::MakeSessionOptions(modelPath, ep, dCuda, dDev);
        m_impl->depthSession = std::make_unique<Ort::Session>(m_impl->env, modelPath.c_str(), options);

        m_impl->depthInNames.clear();
        m_impl->depthOutNames.clear();
        for (size_t i = 0; i < m_impl->depthSession->GetInputCount(); ++i)
        {
            m_impl->depthInNames.emplace_back(
                m_impl->depthSession->GetInputNameAllocated(i, m_impl->alloc).get());
        }
        for (size_t i = 0; i < m_impl->depthSession->GetOutputCount(); ++i)
        {
            m_impl->depthOutNames.emplace_back(
                m_impl->depthSession->GetOutputNameAllocated(i, m_impl->alloc).get());
        }

        // Processing long-side: $PROJECTM_SEG_DEPTH_SIZE > caller > 392. Per-frame W/H are derived
        // from this and the frame aspect (each snapped to a multiple of 14) in ApplyDepthGate.
        const int reqSize = (size > 0) ? size : 392;
        m_impl->depthSize = std::max(14, EnvInt("PROJECTM_SEG_DEPTH_SIZE", reqSize));
        m_impl->depthBand =
            std::clamp(EnvFloat("PROJECTM_SEG_DEPTH_BAND", band > 0.0f ? band : 0.20f), 0.0f, 1.0f);
        m_impl->depthInvert = invert || (EnvInt("PROJECTM_SEG_DEPTH_INVERT", 0) != 0);

        SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                    "[SegMasker] Depth gate loaded '%s' (long-side %d, band %.2f, invert %d).",
                    modelPath.c_str(), m_impl->depthSize, m_impl->depthBand,
                    m_impl->depthInvert ? 1 : 0);
        return true;
    }
    catch (const std::exception& e)
    {
        SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION,
                    "[SegMasker] Depth model '%s' failed to load (%s); depth gate disabled.",
                    modelPath.c_str(), e.what());
        m_impl->depthSession.reset();
        return false;
    }
}

void SegMasker::SetHarden(float lo, float hi)
{
    m_impl->hardenLo = lo;
    m_impl->hardenHi = hi;
}

bool SegMasker::IsLoaded() const
{
    return m_impl->session != nullptr;
}

bool SegMasker::HasDepth() const
{
    return !m_impl->depthMap.empty() && m_impl->depthMapW > 0 && m_impl->depthMapH > 0;
}

float SegMasker::SampleDepth(float fx, float fy) const
{
    const Impl& I = *m_impl;
    if (I.depthMap.empty() || I.depthMapW <= 0 || I.depthMapH <= 0)
    {
        return -1.0f;
    }
    // fy is bottom-up; the depth map's row 0 is the top of the frame.
    const float cx = std::clamp(fx, 0.0f, 1.0f) * static_cast<float>(I.depthMapW - 1);
    const float ry = std::clamp(1.0f - fy, 0.0f, 1.0f) * static_cast<float>(I.depthMapH - 1);
    const int x0 = static_cast<int>(std::floor(cx));
    const int y0 = static_cast<int>(std::floor(ry));
    const int x1 = std::min(x0 + 1, I.depthMapW - 1);
    const int y1 = std::min(y0 + 1, I.depthMapH - 1);
    const float wx = cx - x0;
    const float wy = ry - y0;
    const float* d = I.depthMap.data();
    const float top = d[y0 * I.depthMapW + x0] * (1 - wx) + d[y0 * I.depthMapW + x1] * wx;
    const float bot = d[y1 * I.depthMapW + x0] * (1 - wx) + d[y1 * I.depthMapW + x1] * wx;
    return top * (1 - wy) + bot * wy;
}

void SegMasker::Process(const uint8_t* bgra, int w, int h, bool mirror,
                        std::vector<uint8_t>& outRGBA)
{
    // Per-stage timing (see SegTimings / SEG_MASK_PERF.md). Process() has several early returns,
    // so totalMs is closed out by a scope guard rather than a line at the end.
    using Clock = std::chrono::steady_clock;
    auto elapsedMs = [](Clock::time_point from, Clock::time_point to) {
        return std::chrono::duration<double, std::milli>(to - from).count();
    };
    struct TotalTimer
    {
        Clock::time_point start;
        double& out;
        ~TotalTimer()
        {
            out = std::chrono::duration<double, std::milli>(Clock::now() - start).count();
        }
    };
    m_impl->timings = SegTimings{};
    const auto tStart = Clock::now();
    const TotalTimer totalTimer{tStart, m_impl->timings.totalMs};

    outRGBA.resize(static_cast<size_t>(w) * h * 4);
    m_impl->rgbBuf.resize(static_cast<size_t>(w) * h * 3);

    // Build the (optionally mirrored) RGB output frame the matte will align to. The pose tracker
    // reuses this buffer via RgbFrame() instead of redoing the identical conversion.
    for (int y = 0; y < h; ++y)
    {
        for (int x = 0; x < w; ++x)
        {
            const int srcX = mirror ? (w - 1 - x) : x;
            const uint8_t* s = bgra + (static_cast<size_t>(y) * w + srcX) * 4;
            uint8_t* rgb = m_impl->rgbBuf.data() + (static_cast<size_t>(y) * w + x) * 3;
            rgb[0] = s[2];
            rgb[1] = s[1];
            rgb[2] = s[0];
        }
    }
    const auto tRgbDone = Clock::now();
    m_impl->timings.rgbMs = elapsedMs(tStart, tRgbDone);

    auto passthrough = [&]() {
        for (int i = 0; i < w * h; ++i)
        {
            outRGBA[i * 4 + 0] = m_impl->rgbBuf[i * 3 + 0];
            outRGBA[i * 4 + 1] = m_impl->rgbBuf[i * 3 + 1];
            outRGBA[i * 4 + 2] = m_impl->rgbBuf[i * 3 + 2];
            outRGBA[i * 4 + 3] = 255;
        }
    };

    // Run the optional secondary model on the same frame and multiply its matte into ours
    // (e.g. RVM soft matte x person mask = soft, people-only matte). Call on success paths.
    auto multiplySecondary = [&]() {
        if (!m_impl->secondary) { return; }
        m_impl->secondary->Process(bgra, w, h, mirror, m_impl->secondaryRGBA);
        if (m_impl->secondaryRGBA.size() != outRGBA.size()) { return; }
        if (m_impl->secondaryGate)
        {
            // Soft gate: pass the primary through where the secondary mask is present, reject
            // (zero) where it's clearly absent. A narrow ramp at the threshold means the
            // primary's edges survive across the subject -- only non-subject regions get cut.
            const float lo = m_impl->gateThresh - 0.05f;
            const float hi = m_impl->gateThresh + 0.05f;
            for (int i = 0; i < w * h; ++i)
            {
                const float s = m_impl->secondaryRGBA[i * 4 + 3] / 255.0f;
                float g = std::clamp((s - lo) / (hi - lo), 0.0f, 1.0f);
                g = g * g * (3.0f - 2.0f * g); // smoothstep
                outRGBA[i * 4 + 3] = static_cast<uint8_t>(outRGBA[i * 4 + 3] * g + 0.5f);
            }
        }
        else
        {
            for (int i = 0; i < w * h; ++i)
            {
                outRGBA[i * 4 + 3] = static_cast<uint8_t>(
                    (outRGBA[i * 4 + 3] * m_impl->secondaryRGBA[i * 4 + 3]) / 255);
            }
        }
    };

    if (!m_impl->session)
    {
        passthrough();
        return;
    }

    if (m_impl->yolo)
    {
        const int size = m_impl->inW; // square model input
        float lbScale = 1.0f;
        int padX = 0, padY = 0;
        onnxcommon::RgbToChwLetterbox(m_impl->rgbBuf.data(), w, h, size, m_impl->inputBuf.data(),
                          lbScale, padX, padY);

        Ort::MemoryInfo memInfo = Ort::MemoryInfo::CreateCpu(OrtArenaAllocator, OrtMemTypeDefault);
        const std::array<int64_t, 4> srcShape{1, 3, size, size};
        Ort::Value srcTensor = Ort::Value::CreateTensor<float>(
            memInfo, m_impl->inputBuf.data(), m_impl->inputBuf.size(), srcShape.data(), srcShape.size());

        std::vector<Ort::Value> outputs;
        try
        {
            const char* inName = m_impl->inNames[0].c_str();
            std::vector<const char*> outPtrs;
            for (const auto& n : m_impl->outNames) { outPtrs.push_back(n.c_str()); }
            outputs = m_impl->session->Run(Ort::RunOptions{nullptr}, &inName, &srcTensor, 1,
                                           outPtrs.data(), outPtrs.size());
        }
        catch (const std::exception& e)
        {
            SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION, "[SegMasker] YOLO run failed: %s", e.what());
            passthrough();
            return;
        }

        // Identify the detection (3-D) and mask-prototype (4-D) outputs by rank.
        const float* det = nullptr;
        int detD1 = 0, detD2 = 0;
        const float* proto = nullptr;
        int pc = 0, ph = 0, pw = 0;
        for (auto& o : outputs)
        {
            const auto s = o.GetTensorTypeAndShapeInfo().GetShape();
            if (s.size() == 3)
            {
                det = o.GetTensorMutableData<float>();
                detD1 = static_cast<int>(s[1]);
                detD2 = static_cast<int>(s[2]);
            }
            else if (s.size() == 4)
            {
                proto = o.GetTensorMutableData<float>();
                pc = static_cast<int>(s[1]);
                ph = static_cast<int>(s[2]);
                pw = static_cast<int>(s[3]);
            }
        }
        if (!det || !proto)
        {
            passthrough();
            return;
        }

        // Orientation: standard export is [1, nf, na] (features first); accept [1, na, nf] too.
        int nf = 0, na = 0, strideF = 0, strideA = 0;
        if (detD1 <= detD2) { nf = detD1; na = detD2; strideF = na; strideA = 1; }
        else { nf = detD2; na = detD1; strideF = 1; strideA = nf; }
        const int nc = nf - 4 - pc; // class count (4 bbox + nc classes + pc mask coeffs)
        if (nc < 1)
        {
            passthrough();
            return;
        }

        const float conf = EnvFloat("PROJECTM_SEG_CONF", 0.35f);
        const int personCls = 0; // COCO "person"
        auto val = [&](int f, int a) { return det[f * strideF + a * strideA]; };

        // Collect person detections (boxes in model-pixel space) and their mask coefficients.
        std::vector<YoloDet> dets;
        std::vector<float> coeffs;
        for (int a = 0; a < na; ++a)
        {
            float best = -1.0f;
            int bestCls = 0;
            for (int c = 0; c < nc; ++c)
            {
                const float s = val(4 + c, a);
                if (s > best) { best = s; bestCls = c; }
            }
            if (bestCls != personCls || best < conf) { continue; }
            YoloDet d{};
            d.cx = val(0, a); d.cy = val(1, a); d.w = val(2, a); d.h = val(3, a);
            d.score = best;
            d.coeffOffset = static_cast<int>(coeffs.size());
            for (int k = 0; k < pc; ++k) { coeffs.push_back(val(4 + nc + k, a)); }
            dets.push_back(d);
        }

        // NMS.
        std::sort(dets.begin(), dets.end(),
                  [](const YoloDet& a, const YoloDet& b) { return a.score > b.score; });
        std::vector<char> removed(dets.size(), 0);
        std::vector<YoloDet> keep;
        for (size_t i = 0; i < dets.size(); ++i)
        {
            if (removed[i]) { continue; }
            keep.push_back(dets[i]);
            for (size_t j = i + 1; j < dets.size(); ++j)
            {
                if (!removed[j] &&
                    onnxcommon::BoxIoU(dets[i].cx, dets[i].cy, dets[i].w, dets[i].h,
                                       dets[j].cx, dets[j].cy, dets[j].w, dets[j].h) > 0.45f)
                {
                    removed[j] = 1;
                }
            }
        }

        // Assemble the soft person mask at proto resolution: union of per-instance masks
        // (coeffs . prototypes -> sigmoid), each cropped to its detection box.
        m_impl->yoloMask.assign(static_cast<size_t>(pw) * ph, 0.0f);
        const float protoScale = static_cast<float>(pw) / size; // model px -> proto px
        const int protoPlane = pw * ph;
        for (const auto& d : keep)
        {
            const float* cf = coeffs.data() + d.coeffOffset;
            const int bx0 = std::clamp(static_cast<int>(std::floor((d.cx - d.w * 0.5f) * protoScale)), 0, pw - 1);
            const int by0 = std::clamp(static_cast<int>(std::floor((d.cy - d.h * 0.5f) * protoScale)), 0, ph - 1);
            const int bx1 = std::clamp(static_cast<int>(std::ceil((d.cx + d.w * 0.5f) * protoScale)), 0, pw - 1);
            const int by1 = std::clamp(static_cast<int>(std::ceil((d.cy + d.h * 0.5f) * protoScale)), 0, ph - 1);
            for (int y = by0; y <= by1; ++y)
            {
                for (int x = bx0; x <= bx1; ++x)
                {
                    const int pi = y * pw + x;
                    float acc = 0.0f;
                    for (int k = 0; k < pc; ++k) { acc += cf[k] * proto[k * protoPlane + pi]; }
                    const float m = Sigmoid(acc);
                    if (m > m_impl->yoloMask[pi]) { m_impl->yoloMask[pi] = m; }
                }
            }
        }

        // Write RGB + alpha, mapping each frame pixel through the letterbox into proto space.
        for (int y = 0; y < h; ++y)
        {
            const float my = ((y + 0.5f) * lbScale + padY) * protoScale - 0.5f;
            for (int x = 0; x < w; ++x)
            {
                const size_t di = static_cast<size_t>(y) * w + x;
                outRGBA[di * 4 + 0] = m_impl->rgbBuf[di * 3 + 0];
                outRGBA[di * 4 + 1] = m_impl->rgbBuf[di * 3 + 1];
                outRGBA[di * 4 + 2] = m_impl->rgbBuf[di * 3 + 2];
                const float mx = ((x + 0.5f) * lbScale + padX) * protoScale - 0.5f;
                const float a = SampleMatte(m_impl->yoloMask.data(), pw, ph, mx, my);
                outRGBA[di * 4 + 3] = static_cast<uint8_t>(std::clamp(a, 0.0f, 1.0f) * 255.0f + 0.5f);
            }
        }
        multiplySecondary();
        HardenAlpha(w, h, outRGBA);
        ApplyDepthGate(w, h, outRGBA);
        return;
    }

    const int mw = m_impl->inW;
    const int mh = m_impl->inH;
    if (m_impl->u2net)
    {
        RgbToChwImageNet(m_impl->rgbBuf.data(), w, h, mw, mh, m_impl->inputBuf.data());
    }
    else
    {
        RgbToChw(m_impl->rgbBuf.data(), w, h, mw, mh, m_impl->normScale, m_impl->normBias,
                 m_impl->inputBuf.data());
    }

    Ort::MemoryInfo memInfo = Ort::MemoryInfo::CreateCpu(OrtArenaAllocator, OrtMemTypeDefault);
    const std::array<int64_t, 4> srcShape{1, 3, mh, mw};
    Ort::Value srcTensor = Ort::Value::CreateTensor<float>(
        memInfo, m_impl->inputBuf.data(), m_impl->inputBuf.size(),
        srcShape.data(), srcShape.size());

    float* matte = nullptr;
    int matteW = mw;
    int matteH = mh;
    std::vector<Ort::Value> outputs;

    try
    {
        if (m_impl->rvm && m_impl->useCuda)
        {
            // CUDA path: drive Run() through the IoBinding so RVM's recurrent state stays on the
            // GPU. r*i are bound from the device tensors produced as last frame's r*o (zero-filled
            // host tensors on the first frame; ORT copies those up once), and r*o are bound back to
            // CUDA device memory — so the hidden state never round-trips to host. Only the matte
            // (pha) is read back to the CPU for compositing.
            m_impl->ratioVal = m_impl->downsampleRatio;
            const std::array<int64_t, 1> ratioShape{1};
            Ort::Value ratioTensor = Ort::Value::CreateTensor<float>(
                memInfo, &m_impl->ratioVal, 1, ratioShape.data(), ratioShape.size());

            auto& b = *m_impl->binding;
            b.ClearBoundInputs();
            b.ClearBoundOutputs();
            for (const auto& name : m_impl->inNames)
            {
                if (name == "src")
                {
                    b.BindInput(name.c_str(), srcTensor);
                }
                else if (name == "downsample_ratio")
                {
                    b.BindInput(name.c_str(), ratioTensor);
                }
                else // r1i..r4i — device-resident (host zeros only on the first frame)
                {
                    const int k = std::clamp(name[1] - '1', 0, 3);
                    b.BindInput(name.c_str(), m_impl->recurrent[k]);
                }
            }
            // pha -> host (read back for compositing); r*o -> CUDA device (kept for next frame).
            const Ort::MemoryInfo cudaMem("Cuda", OrtArenaAllocator, m_impl->cudaDevice, OrtMemTypeDefault);
            const Ort::MemoryInfo phaOut = Ort::MemoryInfo::CreateCpu(OrtArenaAllocator, OrtMemTypeCPUOutput);
            for (const auto& n : m_impl->outNames)
            {
                b.BindOutput(n.c_str(), (n == "pha") ? phaOut : cudaMem);
            }

            m_impl->session->Run(Ort::RunOptions{nullptr}, b);
            outputs = b.GetOutputValues(); // in bind order == outNames order

            for (size_t i = 0; i < m_impl->outNames.size() && i < outputs.size(); ++i)
            {
                const std::string& n = m_impl->outNames[i];
                if (n == "pha")
                {
                    matte = outputs[i].GetTensorMutableData<float>();
                    const auto os = outputs[i].GetTensorTypeAndShapeInfo().GetShape();
                    if (os.size() >= 2)
                    {
                        matteH = static_cast<int>(os[os.size() - 2]);
                        matteW = static_cast<int>(os[os.size() - 1]);
                    }
                }
                else if (n.size() == 3 && n[0] == 'r' && n[2] == 'o')
                {
                    const int k = std::clamp(n[1] - '1', 0, 3);
                    m_impl->recurrent[k] = std::move(outputs[i]); // device -> next frame's r*i
                }
            }
        }
        else if (m_impl->rvm)
        {
            // Build inputs by name: src / r1i..r4i / downsample_ratio.
            m_impl->ratioVal = m_impl->downsampleRatio;
            const std::array<int64_t, 1> ratioShape{1};
            std::vector<const char*> inPtrs;
            std::vector<Ort::Value> inVals;
            for (const auto& name : m_impl->inNames)
            {
                inPtrs.push_back(name.c_str());
                if (name == "src")
                {
                    inVals.push_back(std::move(srcTensor));
                }
                else if (name == "downsample_ratio")
                {
                    inVals.push_back(Ort::Value::CreateTensor<float>(
                        memInfo, &m_impl->ratioVal, 1, ratioShape.data(), ratioShape.size()));
                }
                else // r1i..r4i
                {
                    const int k = name[1] - '1';
                    inVals.push_back(std::move(m_impl->recurrent[std::clamp(k, 0, 3)]));
                }
            }
            std::vector<const char*> outPtrs;
            outPtrs.reserve(m_impl->outNames.size());
            for (const auto& n : m_impl->outNames)
            {
                outPtrs.push_back(n.c_str());
            }

            outputs = m_impl->session->Run(Ort::RunOptions{nullptr}, inPtrs.data(),
                                           inVals.data(), inVals.size(),
                                           outPtrs.data(), outPtrs.size());

            // Pick pha; carry r*o back into the recurrent state.
            for (size_t i = 0; i < m_impl->outNames.size(); ++i)
            {
                const std::string& n = m_impl->outNames[i];
                if (n == "pha")
                {
                    matte = outputs[i].GetTensorMutableData<float>();
                    const auto os = outputs[i].GetTensorTypeAndShapeInfo().GetShape();
                    if (os.size() >= 2)
                    {
                        matteH = static_cast<int>(os[os.size() - 2]);
                        matteW = static_cast<int>(os[os.size() - 1]);
                    }
                }
                else if (n.size() == 3 && n[0] == 'r' && n[2] == 'o')
                {
                    const int k = std::clamp(n[1] - '1', 0, 3);
                    m_impl->recurrent[k] = std::move(outputs[i]);
                }
            }
        }
        else
        {
            const char* inName = m_impl->inNames[0].c_str();
            const char* outName = m_impl->outNames[0].c_str();
            outputs = m_impl->session->Run(Ort::RunOptions{nullptr}, &inName,
                                           &srcTensor, 1, &outName, 1);
            matte = outputs[0].GetTensorMutableData<float>();
            const auto os = outputs[0].GetTensorTypeAndShapeInfo().GetShape();
            if (os.size() >= 2)
            {
                matteH = static_cast<int>(os[os.size() - 2]);
                matteW = static_cast<int>(os[os.size() - 1]);
            }
        }
    }
    catch (const std::exception& e)
    {
        SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION, "[SegMasker] Run failed: %s", e.what());
        passthrough();
        return;
    }

    if (!matte)
    {
        passthrough();
        return;
    }

    // U²-Net's output is a saliency map; stretch it to [0,1] per frame (matches rembg) so the
    // strongest response becomes full foreground and the weakest full background.
    if (m_impl->u2net)
    {
        const int n = matteW * matteH;
        float mi = matte[0];
        float ma = matte[0];
        for (int i = 1; i < n; ++i)
        {
            mi = std::min(mi, matte[i]);
            ma = std::max(ma, matte[i]);
        }
        const float inv = (ma - mi) > 1e-6f ? 1.0f / (ma - mi) : 1.0f;
        for (int i = 0; i < n; ++i)
        {
            matte[i] = (matte[i] - mi) * inv;
        }
    }

    // Everything from the RGB build to here is preprocess + the ONNX Run itself.
    const auto tInferDone = Clock::now();
    m_impl->timings.inferMs = elapsedMs(tRgbDone, tInferDone);

    const float rx = static_cast<float>(matteW) / w;
    const float ry = static_cast<float>(matteH) / h;
    for (int y = 0; y < h; ++y)
    {
        const float fy = (y + 0.5f) * ry - 0.5f;
        for (int x = 0; x < w; ++x)
        {
            const size_t di = static_cast<size_t>(y) * w + x;
            outRGBA[di * 4 + 0] = m_impl->rgbBuf[di * 3 + 0];
            outRGBA[di * 4 + 1] = m_impl->rgbBuf[di * 3 + 1];
            outRGBA[di * 4 + 2] = m_impl->rgbBuf[di * 3 + 2];
            const float a = SampleMatte(matte, matteW, matteH, (x + 0.5f) * rx - 0.5f, fy);
            outRGBA[di * 4 + 3] = static_cast<uint8_t>(std::clamp(a, 0.0f, 1.0f) * 255.0f + 0.5f);
        }
    }
    multiplySecondary();
    const auto tCompositeDone = Clock::now();
    m_impl->timings.compositeMs = elapsedMs(tInferDone, tCompositeDone);

    HardenAlpha(w, h, outRGBA);
    const auto tHardenDone = Clock::now();
    m_impl->timings.hardenMs = elapsedMs(tCompositeDone, tHardenDone);

    ApplyDepthGate(w, h, outRGBA);
    m_impl->timings.depthMs = elapsedMs(tHardenDone, Clock::now());
}

const SegTimings& SegMasker::LastTimings() const
{
    return m_impl->timings;
}

const uint8_t* SegMasker::RgbFrame() const
{
    return m_impl->rgbBuf.empty() ? nullptr : m_impl->rgbBuf.data();
}

// Contrast-harden the matte alpha through smoothstep(lo, hi, a). RVM (and other matting models)
// emit soft, partial alpha at edges and uncertain regions, which shows as translucent "ghosting"
// over a hard composite. Remapping with a steep smoothstep snaps mid values toward 0/1: alpha <= lo
// becomes fully transparent, alpha >= hi fully opaque, with a smooth ramp between. lo==hi gives a
// hard threshold. Tuned live via $PROJECTM_SEG_HARDEN_LO / _HI; the default lo=0,hi=1 is a no-op so
// the raw matte passes through unchanged.
void SegMasker::HardenAlpha(int w, int h, std::vector<uint8_t>& outRGBA)
{
    const float lo = EnvFloat("PROJECTM_SEG_HARDEN_LO", m_impl->hardenLo);
    const float hi = EnvFloat("PROJECTM_SEG_HARDEN_HI", m_impl->hardenHi);
    if (lo <= 0.0f && hi >= 1.0f) { return; } // identity -> leave the matte untouched
    const float span = std::max(hi - lo, 1e-4f); // lo==hi -> near-hard threshold
    for (int i = 0; i < w * h; ++i)
    {
        const float t = std::clamp((outRGBA[i * 4 + 3] / 255.0f - lo) / span, 0.0f, 1.0f);
        const float a = t * t * (3.0f - 2.0f * t); // smoothstep
        outRGBA[i * 4 + 3] = static_cast<uint8_t>(a * 255.0f + 0.5f);
    }
}

// Monocular depth gate: run the depth model on the just-built RGB frame, split the matte into
// connected components, measure each component's median relative depth, and fade out the alpha of
// components sitting far behind the nearest one. The whole decision is made on a coarse grid (so a
// per-pixel-noisy depth map still yields a stable per-person verdict) and applied back to the
// full-res alpha through a bilinearly-sampled weight grid, which feathers the cut. Relative depth
// is sufficient -- we only rank components, never use metric distance. See LoadDepth.
void SegMasker::ApplyDepthGate(int w, int h, std::vector<uint8_t>& outRGBA)
{
    auto& I = *m_impl;
    if (!I.depthSession || w <= 0 || h <= 0) { return; }

    // 1. Depth input dims: aspect-matched to the frame, each snapped to a multiple of 14 (DINOv2
    //    patch size), long side = depthSize. Resize the scratch only when the dims change.
    auto snap14 = [](int v) { return std::max(14, (v / 14) * 14); };
    int dW, dH;
    if (w >= h) { dW = snap14(I.depthSize); dH = snap14(std::max(1, I.depthSize * h / w)); }
    else        { dH = snap14(I.depthSize); dW = snap14(std::max(1, I.depthSize * w / h)); }
    if (dW != I.depthW || dH != I.depthH)
    {
        I.depthW = dW;
        I.depthH = dH;
        I.depthInput.resize(static_cast<size_t>(3) * dW * dH);
    }

    // 2. Preprocess (ImageNet) and run the depth model.
    RgbToChwImageNetStd(I.rgbBuf.data(), w, h, dW, dH, I.depthInput.data());
    Ort::MemoryInfo memInfo = Ort::MemoryInfo::CreateCpu(OrtArenaAllocator, OrtMemTypeDefault);
    const std::array<int64_t, 4> inShape{1, 3, dH, dW};
    Ort::Value inTensor = Ort::Value::CreateTensor<float>(
        memInfo, I.depthInput.data(), I.depthInput.size(), inShape.data(), inShape.size());

    std::vector<Ort::Value> outputs;
    try
    {
        const char* inName = I.depthInNames[0].c_str();
        const char* outName = I.depthOutNames[0].c_str();
        outputs = I.depthSession->Run(Ort::RunOptions{nullptr}, &inName, &inTensor, 1, &outName, 1);
    }
    catch (const std::exception& e)
    {
        SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION, "[SegMasker] Depth run failed: %s", e.what());
        return; // leave the matte unchanged
    }

    const float* depth = outputs[0].GetTensorMutableData<float>();
    const auto os = outputs[0].GetTensorTypeAndShapeInfo().GetShape();
    const int mh = (os.size() >= 2) ? static_cast<int>(os[os.size() - 2]) : 0;
    const int mw = (os.size() >= 2) ? static_cast<int>(os[os.size() - 1]) : 0;
    if (mw <= 0 || mh <= 0) { return; }
    I.depthMapW = mw;
    I.depthMapH = mh;

    // Scene depth span, used to normalize closeness to [0,1] (1 = nearest). Scale-free, so no
    // calibration: the keep band is a fraction of the full near-to-far spread of the frame.
    float dmin = depth[0], dmax = depth[0];
    const int dn = mw * mh;
    for (int i = 1; i < dn; ++i)
    {
        dmin = std::min(dmin, depth[i]);
        dmax = std::max(dmax, depth[i]);
    }
    const float drange = (dmax - dmin) > 1e-6f ? (dmax - dmin) : 1.0f;
    auto closeness = [&](float v) {
        return I.depthInvert ? (dmax - v) / drange : (v - dmin) / drange;
    };

    // Retain the full-resolution closeness map so callers can sample depth at an arbitrary point
    // (e.g. a wrist keypoint) via SampleDepth. Stored as closeness in [0,1] (1 = nearest).
    I.depthMap.resize(static_cast<size_t>(dn));
    for (int i = 0; i < dn; ++i) { I.depthMap[i] = closeness(depth[i]); }

    // 3. Coarse labeling grid (aspect-matched to the frame). Each cell samples the matte alpha
    //    (foreground?) and the depth map at its center.
    const int gridLong = std::clamp(EnvInt("PROJECTM_SEG_DEPTH_GRID", 384), 16, 512);
    int gw, gh;
    if (w >= h) { gw = gridLong; gh = std::max(1, gridLong * h / w); }
    else        { gh = gridLong; gw = std::max(1, gridLong * w / h); }
    const int gn = gw * gh;
    I.gateW = gw;
    I.gateH = gh;
    I.ccLabel.assign(gn, -1);
    I.cellWeight.assign(gn, 1.0f); // background / kept default = 1 (alpha unchanged)

    const float alphaThresh =
        std::clamp(EnvFloat("PROJECTM_SEG_DEPTH_ALPHA", 0.5f), 0.0f, 1.0f) * 255.0f;
    std::vector<float> cellDepth(gn, 0.0f);
    std::vector<char> cellFg(gn, 0);
    for (int gy = 0; gy < gh; ++gy)
    {
        for (int gx = 0; gx < gw; ++gx)
        {
            const int ci = gy * gw + gx;
            const int px = std::min(w - 1, static_cast<int>((gx + 0.5f) / gw * w));
            const int py = std::min(h - 1, static_cast<int>((gy + 0.5f) / gh * h));
            cellFg[ci] = (outRGBA[(static_cast<size_t>(py) * w + px) * 4 + 3] >= alphaThresh) ? 1 : 0;
            const int dx = std::min(mw - 1, static_cast<int>((gx + 0.5f) / gw * mw));
            const int dy = std::min(mh - 1, static_cast<int>((gy + 0.5f) / gh * mh));
            cellDepth[ci] = depth[dy * mw + dx];
        }
    }

    // 4. Connected components (4-connectivity) over foreground cells; collect each one's depths
    //    and accumulate its centroid (grid coords) so step 5 can weigh size and centrality.
    std::vector<std::vector<float>> compDepths;
    std::vector<double> compSumX, compSumY; // centroid accumulators, parallel to compDepths
    std::vector<int> stack;
    for (int s = 0; s < gn; ++s)
    {
        if (!cellFg[s] || I.ccLabel[s] >= 0) { continue; }
        const int label = static_cast<int>(compDepths.size());
        compDepths.emplace_back();
        compSumX.push_back(0.0);
        compSumY.push_back(0.0);
        stack.clear();
        stack.push_back(s);
        I.ccLabel[s] = label;
        while (!stack.empty())
        {
            const int c = stack.back();
            stack.pop_back();
            compDepths[label].push_back(cellDepth[c]);
            const int cx = c % gw, cy = c / gw;
            compSumX[label] += cx;
            compSumY[label] += cy;
            const int nb[4][2] = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};
            for (const auto& d : nb)
            {
                const int nx = cx + d[0], ny = cy + d[1];
                if (nx < 0 || ny < 0 || nx >= gw || ny >= gh) { continue; }
                const int ni = ny * gw + nx;
                if (cellFg[ni] && I.ccLabel[ni] < 0)
                {
                    I.ccLabel[ni] = label;
                    stack.push_back(ni);
                }
            }
        }
    }
    if (compDepths.empty()) { return; } // no people in the matte -> nothing to gate

    // 5. Reference depth = the closeness of the *anchor* component -- the main subject. Rather than
    //    simply taking the nearest blob (which lets a partial figure clipping the screen edge, even
    //    one only slightly closer, steal the reference and push the real subject behind the keep
    //    band), we score each sizable component by salience = area x centrality and anchor on the
    //    winner. A reasonably large, centred figure therefore outranks a small edge fragment even
    //    when the fragment is nearer; because the keep band still spares everything in front of the
    //    anchor (step 6), that nearer fragment is itself kept -- we only stop it from hijacking the
    //    band. Using each component's median depth keeps the reference robust to stray near specks.
    //    Components smaller than minArea are ignored as noise/fragments.
    const int minArea = std::max(1, EnvInt("PROJECTM_SEG_DEPTH_MINAREA", std::max(4, gn / 400)));
    // Centrality strength: how steeply salience falls off toward the frame edge. 0 disables it
    // (pure largest-blob anchoring); larger values favour the middle of the screen more strongly.
    const float centerBias = std::max(0.0f, EnvFloat("PROJECTM_SEG_DEPTH_CENTER", 1.0f));
    const float cx0 = 0.5f * (gw - 1), cy0 = 0.5f * (gh - 1);
    const float halfDiag = std::max(1.0f, std::sqrt(cx0 * cx0 + cy0 * cy0));
    std::vector<float> compClose(compDepths.size(), -1.0f); // kept for the debug log
    std::vector<float> compSal(compDepths.size(), -1.0f);   // kept for the debug log
    float refClose = -1.0f;
    int anchorK = -1;
    float bestSalience = -1.0f;
    for (size_t k = 0; k < compDepths.size(); ++k)
    {
        auto& v = compDepths[k];
        const int area = static_cast<int>(v.size());
        if (area < minArea) { continue; }
        std::nth_element(v.begin(), v.begin() + v.size() / 2, v.end());
        const float cl = closeness(v[v.size() / 2]);
        compClose[k] = cl;
        // Centroid distance from frame centre, normalized to [0,1] (0 = dead centre, 1 = corner).
        const float ccx = static_cast<float>(compSumX[k] / area);
        const float ccy = static_cast<float>(compSumY[k] / area);
        const float r = std::sqrt((ccx - cx0) * (ccx - cx0) + (ccy - cy0) * (ccy - cy0)) / halfDiag;
        const float centrality = 1.0f / (1.0f + centerBias * r * r);
        const float salience = static_cast<float>(area) * centrality;
        compSal[k] = salience;
        if (salience > bestSalience) { bestSalience = salience; refClose = cl; anchorK = static_cast<int>(k); }
    }
    if (refClose < 0.0f) { return; } // only noise-sized blobs -> leave the matte unchanged

    // 6. Build a KEEP mask from depth, grow it, then AND it with the matte. Working from "what to
    //    save" rather than "what to cut" is what guarantees the foreground figure is never eroded.
    //    A cell is seeded into the keep mask when it sits at, or nearer than, the target depth
    //    (refClose - band) -- so the subject and anything in front of it are kept, only things
    //    farther are candidates for removal.
    const float ramp = std::max(0.02f, I.depthBand * 0.4f);
    const float keepEdge = refClose - I.depthBand;
    std::vector<float> keep(gn, 0.0f);
    for (int c = 0; c < gn; ++c)
    {
        const float t = std::clamp((closeness(cellDepth[c]) - (keepEdge - ramp)) / (2.0f * ramp),
                                   0.0f, 1.0f);
        keep[c] = t * t * (3.0f - 2.0f * t); // ~1 at/nearer than target, ~0 well behind it
    }

    // Grow the keep region a few cells (4-neighbour max). This is the crucial step: monocular depth
    // bleeds across the subject's silhouette, so a thin shell of the subject's own boundary reads
    // "far" and falls outside the seed -- growing the keep mask covers that shell (and a background
    // margin) so that AND-ing with the matte below leaves every kept edge defined by the full-res
    // matte alone, never carved by the coarse depth grid. A far object stays out of the keep mask
    // unless it sits within `grow` cells of the subject (a touching object keeps a small sliver).
    const int grow = std::clamp(EnvInt("PROJECTM_SEG_DEPTH_GROW", 1), 0, 12);
    for (int it = 0; it < grow; ++it)
    {
        const std::vector<float> prev = keep;
        for (int c = 0; c < gn; ++c)
        {
            const int cx = c % gw, cy = c / gw;
            float d = prev[c];
            if (cx > 0)      { d = std::max(d, prev[c - 1]); }
            if (cx < gw - 1) { d = std::max(d, prev[c + 1]); }
            if (cy > 0)      { d = std::max(d, prev[c - gw]); }
            if (cy < gh - 1) { d = std::max(d, prev[c + gw]); }
            keep[c] = d;
        }
    }
    // AND with the matte happens in step 7, where alpha is multiplied by this weight.
    for (int c = 0; c < gn; ++c) { I.cellWeight[c] = keep[c]; }

    // Optional tuning diagnostic ($PROJECTM_SEG_DEPTH_DEBUG=1): every ~60 frames, report the
    // reference closeness, the keep cutoff, and each sizable component's median closeness. The
    // main subject sets refClose; components well below the cutoff are the ones being faded. If the
    // *nearest* component is the one cut, the depth orientation is flipped -- set
    // PROJECTM_SEG_DEPTH_INVERT=1.
    if (EnvInt("PROJECTM_SEG_DEPTH_DEBUG", 0) != 0)
    {
        static int dbgFrame = 0;
        if ((dbgFrame++ % 60) == 0)
        {
            std::string s;
            for (size_t k = 0; k < compDepths.size() && k < 12; ++k)
            {
                if (compClose[k] < 0.0f) { continue; } // skip tiny/ignored
                char buf[72];
                std::snprintf(buf, sizeof(buf), " [%zu:%s close=%.2f sal=%.0f n=%zu]", k,
                              (static_cast<int>(k) == anchorK ? "*" : ""), compClose[k], compSal[k],
                              compDepths[k].size());
                s += buf;
            }
            SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                        "[SegMasker] depth gate: %zu comps, anchor=%d refClose=%.2f, keep>=%.2f;%s",
                        compDepths.size(), anchorK, refClose, refClose - I.depthBand, s.c_str());
        }
    }

    // 7. The weight grid is NOT applied here. Multiplying it into the full-resolution alpha was a
    //    per-pixel bilinear fetch over the whole frame on the capture thread -- measured at ~6ms
    //    per megapixel, i.e. more than the depth model itself at 1080p. The grid is tiny and the
    //    consumer is a GPU sampler, so it is handed to the library instead (GateGrid ->
    //    projectm_video_submit_alpha_gate) and multiplied into the matte during preprocessing, at
    //    texture resolution. The shader's bilinear fetch feathers the cut identically.
    //
    //    CPU consumers of the matte that must see the gate (the alpha-weighted centroid, the
    //    pose->touch wrist confidence) sample it directly via SampleGate.
}

bool SegMasker::HasGate() const
{
    return !m_impl->cellWeight.empty() && m_impl->gateW > 0 && m_impl->gateH > 0;
}

const float* SegMasker::GateGrid(int& gridW, int& gridH) const
{
    const Impl& I = *m_impl;
    if (!HasGate())
    {
        gridW = 0;
        gridH = 0;
        return nullptr;
    }
    gridW = I.gateW;
    gridH = I.gateH;
    return I.cellWeight.data();
}

float SegMasker::SampleGate(float fx, float fy) const
{
    const Impl& I = *m_impl;
    if (!HasGate())
    {
        return 1.0f; // no gate -> keep everything
    }
    // fy is bottom-up (as in SampleDepth); the grid's row 0 is the top of the frame.
    const float gx = std::clamp(fx, 0.0f, 1.0f) * static_cast<float>(I.gateW) - 0.5f;
    const float gy = std::clamp(1.0f - fy, 0.0f, 1.0f) * static_cast<float>(I.gateH) - 0.5f;
    return std::clamp(SampleMatte(I.cellWeight.data(), I.gateW, I.gateH, gx, gy), 0.0f, 1.0f);
}
