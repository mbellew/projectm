/**
 * @file poseTracker.cpp
 * @brief ONNX Runtime YOLO-pose backend (CoreML EP on macOS, CUDA on Linux).
 *
 * Single-shot pose: letterbox the frame, run the model, decode the [1, 4+1+3*K, N]
 * detection tensor into per-person COCO-17 keypoints, NMS, and map back to normalized
 * frame coords (y bottom-up). No recurrent state; a plain per-frame Run.
 *
 * NOTE (tracker-first): the letterbox / NMS / session-options helpers here are copied
 * from segMask.cpp. A later refactor extracts them into a shared onnxCommon unit; until
 * then this file is self-contained so the seg path stays untouched.
 *
 * Compiled only when ENABLE_ONNX_SEG is on and onnxruntime is found.
 */
#include "poseTracker.hpp"

#include "onnxCommon.hpp"

#include <onnxruntime_cxx_api.h>
#include <SDL2/SDL.h>

#include <algorithm>
#include <array>
#include <chrono>
#include <cmath>
#include <string>
#include <vector>

namespace {

using onnxcommon::EnvFloat;
using onnxcommon::EnvInt;

// NMS candidate box carrying the source index into the pose scratch list.
struct Box
{
    float cx, cy, w, h, score;
    int index;
};

} // namespace

struct PoseTracker::Impl
{
    Ort::Env env{ORT_LOGGING_LEVEL_WARNING, "projectm-pose"};
    std::unique_ptr<Ort::Session> session;
    Ort::AllocatorWithDefaultOptions alloc;

    std::vector<std::string> inNames;
    std::vector<std::string> outNames;
    int inW{640};
    int inH{640};
    bool useCuda{false};
    int cudaDevice{0};

    std::vector<uint8_t> rgbBuf;   // interleaved RGB built from the BGRA frame
    std::vector<float> inputBuf;   // letterboxed CHW input scratch
};

PoseTracker::PoseTracker()
    : m_impl(std::make_unique<Impl>())
{
}

PoseTracker::~PoseTracker() = default;

bool PoseTracker::IsSupported()
{
    return true;
}

bool PoseTracker::Load(const std::string& modelPath, int size)
{
    if (modelPath.empty())
    {
        return false;
    }

    int reqSize = EnvInt("PROJECTM_POSE_SIZE", size);
    if (reqSize <= 0)
    {
        reqSize = 640;
    }
    reqSize = ((reqSize + 31) / 32) * 32; // snap to /32 for the conv stride

    try
    {
        bool useCuda = false;
        int cudaDevice = 0;
        const onnxcommon::EpConfig ep{"PoseTracker", "PROJECTM_POSE_COREML", "PROJECTM_POSE_CUDA",
                                      "PROJECTM_POSE_CUDA_DEVICE"};
        Ort::SessionOptions options = onnxcommon::MakeSessionOptions(modelPath, ep, useCuda, cudaDevice);
        m_impl->session = std::make_unique<Ort::Session>(m_impl->env, modelPath.c_str(), options);
        m_impl->useCuda = useCuda;
        m_impl->cudaDevice = cudaDevice;
    }
    catch (const std::exception& e)
    {
        SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION, "[PoseTracker] Failed to load %s: %s",
                    modelPath.c_str(), e.what());
        m_impl->session.reset();
        return false;
    }

    m_impl->inNames.clear();
    m_impl->outNames.clear();
    for (size_t i = 0; i < m_impl->session->GetInputCount(); ++i)
    {
        m_impl->inNames.push_back(m_impl->session->GetInputNameAllocated(i, m_impl->alloc).get());
    }
    for (size_t i = 0; i < m_impl->session->GetOutputCount(); ++i)
    {
        m_impl->outNames.push_back(m_impl->session->GetOutputNameAllocated(i, m_impl->alloc).get());
    }

    // Resolve the (square) input size: fixed from the model shape, else the requested size.
    int inW = reqSize, inH = reqSize;
    try
    {
        const auto shape = m_impl->session->GetInputTypeInfo(0)
                               .GetTensorTypeAndShapeInfo()
                               .GetShape();
        if (shape.size() == 4)
        {
            if (shape[3] > 0) { inW = static_cast<int>(shape[3]); }
            if (shape[2] > 0) { inH = static_cast<int>(shape[2]); }
        }
    }
    catch (const std::exception&)
    {
        // Dynamic axes: keep the requested size.
    }
    m_impl->inW = inW;
    m_impl->inH = inH;
    m_impl->inputBuf.assign(static_cast<size_t>(3) * inW * inH, 0.0f);

    SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                "[PoseTracker] Loaded %s (input %dx%d, %zu outputs).",
                modelPath.c_str(), inW, inH, m_impl->outNames.size());

    // TEMP debug (pose bring-up): dump each output's declared shape so we can confirm the head
    // layout ([1, 4+1+3*17, N] = [1, 56, N]) before trusting the decode. Remove once verified.
    for (size_t i = 0; i < m_impl->outNames.size(); ++i)
    {
        try
        {
            const auto shape = m_impl->session->GetOutputTypeInfo(i)
                                   .GetTensorTypeAndShapeInfo()
                                   .GetShape();
            std::string dims;
            for (size_t d = 0; d < shape.size(); ++d)
            {
                dims += (d ? "x" : "") + std::to_string(shape[d]);
            }
            SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, "[PoseTracker] output[%zu] '%s' shape=[%s]",
                        i, m_impl->outNames[i].c_str(), dims.c_str());
        }
        catch (const std::exception&)
        {
        }
    }
    return true;
}

bool PoseTracker::IsLoaded() const
{
    return m_impl->session != nullptr;
}

void PoseTracker::Process(const uint8_t* bgra, int w, int h, bool mirror,
                          std::vector<PersonPose>& out)
{
    out.clear();
    if (!m_impl->session || w <= 0 || h <= 0)
    {
        return;
    }

    // BGRA -> interleaved RGB (optionally mirrored). Only needed when there is no seg masker to
    // borrow the already-converted frame from -- see ProcessRgb / SegMasker::RgbFrame().
    m_impl->rgbBuf.resize(static_cast<size_t>(w) * h * 3);
    uint8_t* rgb = m_impl->rgbBuf.data();
    for (int y = 0; y < h; ++y)
    {
        for (int x = 0; x < w; ++x)
        {
            const int srcX = mirror ? (w - 1 - x) : x;
            const uint8_t* p = bgra + (static_cast<size_t>(y) * w + srcX) * 4;
            uint8_t* d = rgb + (static_cast<size_t>(y) * w + x) * 3;
            d[0] = p[2]; // R
            d[1] = p[1]; // G
            d[2] = p[0]; // B
        }
    }

    ProcessRgb(m_impl->rgbBuf.data(), w, h, out);
}

void PoseTracker::ProcessRgb(const uint8_t* rgb, int w, int h, std::vector<PersonPose>& out)
{
    out.clear();
    if (!m_impl->session || rgb == nullptr || w <= 0 || h <= 0)
    {
        return;
    }

    // Letterbox into the square model input.
    const int inW = m_impl->inW;
    const int inH = m_impl->inH;
    const int size = std::min(inW, inH); // YOLO-pose is square; guard anyway
    if (static_cast<int>(m_impl->inputBuf.size()) < 3 * size * size)
    {
        m_impl->inputBuf.assign(static_cast<size_t>(3) * size * size, 0.0f);
    }
    float lbScale = 1.0f;
    int padX = 0, padY = 0;
    onnxcommon::RgbToChwLetterbox(rgb, w, h, size, m_impl->inputBuf.data(), lbScale, padX, padY);

    Ort::MemoryInfo memInfo = Ort::MemoryInfo::CreateCpu(OrtArenaAllocator, OrtMemTypeDefault);
    const std::array<int64_t, 4> srcShape{1, 3, size, size};
    Ort::Value srcTensor = Ort::Value::CreateTensor<float>(
        memInfo, m_impl->inputBuf.data(),
        static_cast<size_t>(3) * size * size, srcShape.data(), srcShape.size());

    std::vector<Ort::Value> outputs;
    const auto runStart = std::chrono::steady_clock::now();
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
        SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION, "[PoseTracker] Run failed: %s", e.what());
        return;
    }
    const double runMs = std::chrono::duration<double, std::milli>(
                             std::chrono::steady_clock::now() - runStart)
                             .count();

    // Pose head: a single 3-D detection tensor [1, F, N] (or [1, N, F]). F = 4 bbox +
    // 1 person-conf + 3*K keypoints. Ultralytics applies sigmoid inside the graph, so
    // confidences are already [0,1] and coords are in model pixels.
    const float* det = nullptr;
    int detD1 = 0, detD2 = 0;
    for (auto& o : outputs)
    {
        const auto s = o.GetTensorTypeAndShapeInfo().GetShape();
        if (s.size() == 3)
        {
            det = o.GetTensorMutableData<float>();
            detD1 = static_cast<int>(s[1]);
            detD2 = static_cast<int>(s[2]);
            break;
        }
    }
    if (!det)
    {
        return;
    }

    int nf = 0, na = 0, strideF = 0, strideA = 0;
    if (detD1 <= detD2) { nf = detD1; na = detD2; strideF = na; strideA = 1; }
    else { nf = detD2; na = detD1; strideF = 1; strideA = nf; }
    const int kptCount = (nf - 5) / 3;

    // TEMP debug (pose bring-up): log the decoded head layout once. nf should be 56 (4 bbox +
    // 1 conf + 3*17 kpts), kptCount 17. If this looks wrong the decode below is misreading.
    static bool loggedLayout = false;
    if (!loggedLayout)
    {
        loggedLayout = true;
        SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                    "[PoseTracker] det tensor [1,%d,%d] -> nf=%d na=%d kptCount=%d (expect nf=56, kpt=17)",
                    detD1, detD2, nf, na, kptCount);
    }

    if (kptCount < kKeypointCount)
    {
        SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION,
                    "[PoseTracker] Unexpected output width %d (need >= %d for %d keypoints).",
                    nf, 5 + 3 * kKeypointCount, kKeypointCount);
        return;
    }

    const float conf = EnvFloat("PROJECTM_POSE_CONF", 0.35f);
    auto val = [&](int f, int a) { return det[f * strideF + a * strideA]; };

    // Map a model-pixel coordinate back to normalized frame coords (y bottom-up).
    const float invW = (w > 0) ? 1.0f / static_cast<float>(w) : 0.0f;
    const float invH = (h > 0) ? 1.0f / static_cast<float>(h) : 0.0f;
    auto toNormX = [&](float mx) {
        return std::clamp(((mx - padX) / lbScale) * invW, 0.0f, 1.0f);
    };
    auto toNormY = [&](float my) {
        return std::clamp(1.0f - ((my - padY) / lbScale) * invH, 0.0f, 1.0f);
    };

    // Collect above-threshold detections, then NMS on the boxes (model-pixel space).
    std::vector<PersonPose> cand;
    std::vector<Box> boxes;
    cand.reserve(16);
    for (int a = 0; a < na; ++a)
    {
        const float s = val(4, a);
        if (s < conf) { continue; }

        const float bcx = val(0, a);
        const float bcy = val(1, a);
        const float bw = val(2, a);
        const float bh = val(3, a);

        PersonPose p{};
        p.score = s;
        p.boxX0 = toNormX(bcx - bw * 0.5f);
        p.boxX1 = toNormX(bcx + bw * 0.5f);
        // y flips, so the top edge (smaller model y) becomes the larger normalized y.
        p.boxY1 = toNormY(bcy - bh * 0.5f);
        p.boxY0 = toNormY(bcy + bh * 0.5f);
        for (int k = 0; k < kKeypointCount; ++k)
        {
            const float kx = val(5 + 3 * k, a);
            const float ky = val(6 + 3 * k, a);
            const float kc = val(7 + 3 * k, a);
            p.kpts[k].x = toNormX(kx);
            p.kpts[k].y = toNormY(ky);
            p.kpts[k].conf = std::clamp(kc, 0.0f, 1.0f);
        }

        boxes.push_back(Box{bcx, bcy, bw, bh, s, static_cast<int>(cand.size())});
        cand.push_back(p);
    }

    std::sort(boxes.begin(), boxes.end(), [](const Box& a, const Box& b) { return a.score > b.score; });
    std::vector<char> removed(boxes.size(), 0);
    for (size_t i = 0; i < boxes.size(); ++i)
    {
        if (removed[i]) { continue; }
        out.push_back(cand[boxes[i].index]);
        for (size_t j = i + 1; j < boxes.size(); ++j)
        {
            if (!removed[j] &&
                onnxcommon::BoxIoU(boxes[i].cx, boxes[i].cy, boxes[i].w, boxes[i].h,
                                   boxes[j].cx, boxes[j].cy, boxes[j].w, boxes[j].h) > 0.45f)
            {
                removed[j] = 1;
            }
        }
    }

    // TEMP debug (pose bring-up): throttled per-run summary so we can watch the path work end-to-
    // end -- inference time, raw candidates over threshold, people kept after NMS, and the best
    // person's arm joints in normalized (0..1, y bottom-up) coords. Every N frames via
    // PROJECTM_POSE_DEBUG_EVERY (0 disables). Remove once the bridge is verified.
    const int every = EnvInt("PROJECTM_POSE_DEBUG_EVERY", 0); // 0 = off; set to N to log every N frames
    static int dbgFrame = 0;
    if (every > 0 && (dbgFrame++ % every) == 0)
    {
        if (out.empty())
        {
            SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                        "[PoseDebug] %.1f ms | %zu raw cand | 0 people (thr=%.2f)",
                        runMs, cand.size(), conf);
        }
        else
        {
            const PersonPose& p = out.front();
            const Keypoint& ls = p[Kpt::LeftShoulder];
            const Keypoint& rs = p[Kpt::RightShoulder];
            const Keypoint& le = p[Kpt::LeftElbow];
            const Keypoint& re = p[Kpt::RightElbow];
            const Keypoint& lw = p[Kpt::LeftWrist];
            const Keypoint& rw = p[Kpt::RightWrist];
            SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                        "[PoseDebug] %.1f ms | %zu raw | %zu people | best=%.2f | "
                        "Lsh(%.2f,%.2f)c%.2f Lel(%.2f,%.2f)c%.2f Lwr(%.2f,%.2f)c%.2f | "
                        "Rsh(%.2f,%.2f)c%.2f Rel(%.2f,%.2f)c%.2f Rwr(%.2f,%.2f)c%.2f",
                        runMs, cand.size(), out.size(), p.score,
                        ls.x, ls.y, ls.conf, le.x, le.y, le.conf, lw.x, lw.y, lw.conf,
                        rs.x, rs.y, rs.conf, re.x, re.y, re.conf, rw.x, rw.y, rw.conf);
        }
    }
}
