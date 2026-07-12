/**
 * @file onnxCommon.cpp
 * @brief Shared ONNX Runtime helpers (see onnxCommon.hpp).
 */
#include "onnxCommon.hpp"

#ifdef __APPLE__
#include <coreml_provider_factory.h>
#endif
#include <SDL2/SDL.h>

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <filesystem>
#include <limits>
#include <system_error>
#include <unordered_map>

namespace onnxcommon {

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

float BoxIoU(float acx, float acy, float aw, float ah,
             float bcx, float bcy, float bw, float bh)
{
    const float ax0 = acx - aw * 0.5f, ay0 = acy - ah * 0.5f;
    const float ax1 = acx + aw * 0.5f, ay1 = acy + ah * 0.5f;
    const float bx0 = bcx - bw * 0.5f, by0 = bcy - bh * 0.5f;
    const float bx1 = bcx + bw * 0.5f, by1 = bcy + bh * 0.5f;
    const float ix = std::max(0.0f, std::min(ax1, bx1) - std::max(ax0, bx0));
    const float iy = std::max(0.0f, std::min(ay1, by1) - std::max(ay0, by0));
    const float inter = ix * iy;
    const float uni = aw * ah + bw * bh - inter;
    return uni > 0.0f ? inter / uni : 0.0f;
}

Ort::SessionOptions MakeSessionOptions(const std::string& modelPath, const EpConfig& cfg,
                                       bool& useCuda, int& cudaDevice)
{
    Ort::SessionOptions options;
    options.SetIntraOpNumThreads(2);
    options.SetGraphOptimizationLevel(ORT_ENABLE_ALL);

    // $PROJECTM_ONNX_DUMP=<dir>: write each model's POST-optimization graph there and log ORT's
    // per-node execution-provider placement. ORT decides placement when it builds the session, so
    // this is a static analysis -- it needs no inference run. Any Memcpy node ORT inserts is a
    // host<->device round trip in the middle of the graph: it serializes the pipeline and (per
    // ORT's own warning) blocks CUDA Graph capture.
    static std::string dumpPath; // ORT copies this, but keep it alive regardless
    if (const char* dumpDir = std::getenv("PROJECTM_ONNX_DUMP"); dumpDir != nullptr && dumpDir[0] != '\0')
    {
        std::error_code ec;
        std::filesystem::create_directories(dumpDir, ec);
        dumpPath = (std::filesystem::path(dumpDir) /
                    (std::filesystem::path(modelPath).stem().string() + ".optimized.onnx"))
                       .string();
        options.SetOptimizedModelFilePath(dumpPath.c_str());
        options.SetLogSeverityLevel(0); // VERBOSE: emits the node-placement table
    }
#ifdef __APPLE__
    // CoreML execution provider (ANE/GPU) is macOS-only.
    if (EnvInt(cfg.coremlEnv, 1) != 0)
    {
        try
        {
            // Cache the compiled CoreML model next to the model so later launches load the cached
            // .mlmodelc instead of paying the multi-second graph compile again.
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
            SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION, "[%s] CoreML EP unavailable (%s); using CPU.",
                        cfg.logTag, e.what());
        }
    }
#else
    // NVIDIA CUDA execution provider (throws and falls back to CPU on a CPU-only ORT).
    if (EnvInt(cfg.cudaEnv, 1) != 0)
    {
        try
        {
            OrtCUDAProviderOptions cudaOptions{};
            cudaOptions.device_id = EnvInt(cfg.cudaDeviceEnv, 0);
            cudaOptions.gpu_mem_limit = std::numeric_limits<size_t>::max();
            cudaOptions.cudnn_conv_algo_search = OrtCudnnConvAlgoSearchHeuristic;
            cudaOptions.do_copy_in_default_stream = 1;
            options.AppendExecutionProvider_CUDA(cudaOptions);
            useCuda = true;
            cudaDevice = cudaOptions.device_id;
            SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, "[%s] Using CUDA execution provider (device %d).",
                        cfg.logTag, cudaOptions.device_id);
        }
        catch (const std::exception& e)
        {
            SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION, "[%s] CUDA EP unavailable (%s); using CPU.",
                        cfg.logTag, e.what());
        }
    }
#endif
    return options;
}

} // namespace onnxcommon
