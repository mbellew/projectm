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

#include <onnxruntime_cxx_api.h>
#include <coreml_provider_factory.h>
#include <SDL2/SDL.h>

#include <algorithm>
#include <array>
#include <cmath>
#include <cstdlib>
#include <filesystem>
#include <system_error>
#include <unordered_map>

namespace {

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

// Letterbox (aspect-preserving resize + gray pad) the RGB frame into a square CHW [0,1] buffer,
// the way Ultralytics expects. Records the scale and pad so masks map back to the frame.
void RgbToChwLetterbox(const uint8_t* rgb, int srcW, int srcH, int size, float* out,
                       float& scaleOut, int& padXOut, int& padYOut)
{
    const float scale = std::min(static_cast<float>(size) / srcW, static_cast<float>(size) / srcH);
    const int newW = std::max(1, static_cast<int>(std::round(srcW * scale)));
    const int newH = std::max(1, static_cast<int>(std::round(srcH * scale)));
    const int padX = (size - newW) / 2;
    const int padY = (size - newH) / 2;
    const int plane = size * size;
    const float padVal = 114.0f / 255.0f; // Ultralytics gray padding

    for (int i = 0; i < 3 * plane; ++i) { out[i] = padVal; }

    const float rsx = static_cast<float>(srcW) / newW;
    const float rsy = static_cast<float>(srcH) / newH;
    for (int y = 0; y < newH; ++y)
    {
        const float fy = (y + 0.5f) * rsy - 0.5f;
        const int y0 = std::clamp(static_cast<int>(std::floor(fy)), 0, srcH - 1);
        const int y1 = std::min(y0 + 1, srcH - 1);
        const float wy = std::clamp(fy - y0, 0.0f, 1.0f);
        for (int x = 0; x < newW; ++x)
        {
            const float fx = (x + 0.5f) * rsx - 0.5f;
            const int x0 = std::clamp(static_cast<int>(std::floor(fx)), 0, srcW - 1);
            const int x1 = std::min(x0 + 1, srcW - 1);
            const float wx = std::clamp(fx - x0, 0.0f, 1.0f);
            const uint8_t* p00 = rgb + (static_cast<size_t>(y0) * srcW + x0) * 3;
            const uint8_t* p01 = rgb + (static_cast<size_t>(y0) * srcW + x1) * 3;
            const uint8_t* p10 = rgb + (static_cast<size_t>(y1) * srcW + x0) * 3;
            const uint8_t* p11 = rgb + (static_cast<size_t>(y1) * srcW + x1) * 3;
            const int dstIdx = (y + padY) * size + (x + padX);
            for (int c = 0; c < 3; ++c)
            {
                const float top = p00[c] * (1 - wx) + p01[c] * wx;
                const float bot = p10[c] * (1 - wx) + p11[c] * wx;
                out[c * plane + dstIdx] = (top * (1 - wy) + bot * wy) / 255.0f;
            }
        }
    }
    scaleOut = scale;
    padXOut = padX;
    padYOut = padY;
}

struct YoloDet
{
    float cx, cy, w, h, score;
    int coeffOffset; // start index of this det's mask coefficients in the flat coeff store
};

float BoxIoU(const YoloDet& a, const YoloDet& b)
{
    const float ax0 = a.cx - a.w * 0.5f, ay0 = a.cy - a.h * 0.5f;
    const float ax1 = a.cx + a.w * 0.5f, ay1 = a.cy + a.h * 0.5f;
    const float bx0 = b.cx - b.w * 0.5f, by0 = b.cy - b.h * 0.5f;
    const float bx1 = b.cx + b.w * 0.5f, by1 = b.cy + b.h * 0.5f;
    const float ix = std::max(0.0f, std::min(ax1, bx1) - std::max(ax0, bx0));
    const float iy = std::max(0.0f, std::min(ay1, by1) - std::max(ay0, by0));
    const float inter = ix * iy;
    const float uni = a.w * a.h + b.w * b.h - inter;
    return uni > 0.0f ? inter / uni : 0.0f;
}

int EnvInt(const char* name, int fallback)
{
    const char* v = std::getenv(name);
    return (v && v[0]) ? std::atoi(v) : fallback;
}
float EnvFloat(const char* name, float fallback)
{
    const char* v = std::getenv(name);
    return (v && v[0]) ? static_cast<float>(std::atof(v)) : fallback;
}

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

    // RVM recurrent state (r1i..r4i), carried frame to frame.
    std::vector<Ort::Value> recurrent;
    float ratioVal{1.0f};

    std::vector<float> inputBuf;        // src CHW
    std::vector<uint8_t> rgbBuf;        // interleaved RGB output frame
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
        Ort::SessionOptions options;
        options.SetIntraOpNumThreads(2);
        options.SetGraphOptimizationLevel(ORT_ENABLE_ALL);
        if (EnvInt("PROJECTM_SEG_COREML", 1) != 0)
        {
            try
            {
                // Cache the compiled CoreML model. The first launch still pays the multi-second
                // graph compile, but it writes the result to ModelCacheDirectory and every later
                // launch loads the cached .mlmodelc instead (sub-second). ORT keys the cache on the
                // model + EP options, so it self-invalidates if either changes. Requires the newer
                // string-options CoreML API (ORT >= 1.21) and the MLProgram format. The cache lives
                // next to the model so it travels with the models directory.
                const std::filesystem::path cacheDir =
                    std::filesystem::path(modelPath).parent_path() / "coreml_cache";
                std::error_code ec;
                std::filesystem::create_directories(cacheDir, ec);
                const std::unordered_map<std::string, std::string> coremlOptions{
                    {"ModelFormat", "MLProgram"},
                    {"MLComputeUnits", "ALL"},
                    {"ModelCacheDirectory", cacheDir.string()},
                };
                options.AppendExecutionProvider("CoreML", coremlOptions);
            }
            catch (const std::exception& e)
            {
                SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION,
                            "[SegMasker] CoreML EP unavailable (%s); using CPU.", e.what());
            }
        }

        m_impl->session = std::make_unique<Ort::Session>(m_impl->env, modelPath.c_str(), options);

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

bool SegMasker::IsLoaded() const
{
    return m_impl->session != nullptr;
}

void SegMasker::Process(const uint8_t* bgra, int w, int h, bool mirror,
                        std::vector<uint8_t>& outRGBA)
{
    outRGBA.resize(static_cast<size_t>(w) * h * 4);
    m_impl->rgbBuf.resize(static_cast<size_t>(w) * h * 3);

    // Build the (optionally mirrored) RGB output frame the matte will align to.
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
        RgbToChwLetterbox(m_impl->rgbBuf.data(), w, h, size, m_impl->inputBuf.data(),
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
                if (!removed[j] && BoxIoU(dets[i], dets[j]) > 0.45f) { removed[j] = 1; }
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
        if (m_impl->rvm)
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
}
