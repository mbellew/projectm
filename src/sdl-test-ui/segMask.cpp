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
                Ort::ThrowOnError(OrtSessionOptionsAppendExecutionProvider_CoreML(options, 0));
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
        else
        {
            m_impl->normScale = 1.0f / 127.5f; // MODNet expects [-1,1]
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
                m_impl->inW = m_impl->inH = segSize;
            }
        }

        m_impl->inputBuf.resize(static_cast<size_t>(3) * m_impl->inW * m_impl->inH);
        SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                    "[SegMasker] Loaded '%s' (%s, input %dx%d, ratio %.2f).",
                    modelPath.c_str(), m_impl->rvm ? "RVM" : "single-input",
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

    if (!m_impl->session)
    {
        passthrough();
        return;
    }

    const int mw = m_impl->inW;
    const int mh = m_impl->inH;
    RgbToChw(m_impl->rgbBuf.data(), w, h, mw, mh, m_impl->normScale, m_impl->normBias,
             m_impl->inputBuf.data());

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
}
