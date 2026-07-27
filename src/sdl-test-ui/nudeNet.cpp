/**
 * @file nudeNet.cpp
 * @brief ONNX Runtime NudeNet v3 detector backend (CoreML EP on macOS, CUDA on Linux).
 *
 * Single-shot detect: letterbox the frame, run the YOLOv8 head [1, 4+18, N], and for the
 * handful of classes we expose, take the max detection confidence whose box overlaps the
 * MAIN figure's pose bbox. Those raw per-run scores drive a Schmitt-trigger + glide so the
 * exposed flags are steady near-binary values (covered/0 by default). A separate
 * FACE_FEMALE-vs-FACE_MALE ratio drives the nude_female axis.
 *
 * Compiled only when ENABLE_ONNX_SEG is on and onnxruntime is found. Meant to be called at a
 * THROTTLED rate from the capture thread (a few Hz) -- the hysteresis integrates over calls.
 */
#include "nudeNet.hpp"

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

// NudeNet v3 class indices (full 18-class map is embedded in the model metadata; these are the
// ones we surface). top/rear/front-f/front-m are the exposure flags; the two FACE_* feed gender.
constexpr int kClsButtocksExposed = 2;       // -> nude_rear
constexpr int kClsFemaleBreastExposed = 3;   // -> nude_top
constexpr int kClsFemaleGenitaliaExposed = 4;// -> nude_front_f
constexpr int kClsMaleGenitaliaExposed = 14; // -> nude_front_m
constexpr int kClsFaceFemale = 1;            // -> nude_female (numerator)
constexpr int kClsFaceMale = 12;             // -> nude_female (denominator side)

//! A de-flickered near-binary flag: presence-based hysteresis. Each run reports whether the class
//! was DETECTED (any gated box >= conf) -- not how strong. Latch on after `onN` consecutive
//! detected runs, off after `offN` consecutive misses; the output glides toward the latched 0/1
//! state so presets never see a hard step or per-frame flicker. Robust to the raw confidence
//! magnitude (NudeNet true positives hover ~0.4-0.6 and fluctuate, which an EMA-of-magnitude
//! Schmitt would never reliably latch). Covered (0) is the default.
struct Flag
{
    int onCount{0};    //!< consecutive detected runs
    int offCount{0};   //!< consecutive missed runs
    bool state{false}; //!< latched on/off
    float out{0.0f};   //!< glided near-binary output, [0,1]

    void Update(bool detected, int onN, int offN, float glide)
    {
        if (detected) { onCount++; offCount = 0; if (onCount >= onN) { state = true; } }
        else { offCount++; onCount = 0; if (offCount >= offN) { state = false; } }
        out += ((state ? 1.0f : 0.0f) - out) * glide;
    }
};

} // namespace

struct NudeNet::Impl
{
    Ort::Env env{ORT_LOGGING_LEVEL_WARNING, "projectm-nudenet"};
    std::unique_ptr<Ort::Session> session;
    Ort::AllocatorWithDefaultOptions alloc;

    std::vector<std::string> inNames;
    std::vector<std::string> outNames;
    int inW{320};
    int inH{320};
    bool useCuda{false};
    int cudaDevice{0};

    std::vector<float> inputBuf; // letterboxed CHW input scratch

    Flag top, rear, frontF, frontM;
    float female{0.5f}; // gender axis EMA; 0.5 = no face / unknown

    // Breast-box centers (up to 2) from the last run, for LOCATION refinement (see BreastCount).
    float breastCx[2]{0.5f, 0.5f};
    float breastCy[2]{0.5f, 0.5f};
    float breastScore[2]{0.0f, 0.0f};
    int breastN{0};

    // Best front-genitalia box center from the last run, for GROIN LOCATION refinement (one point).
    float groinCx{0.5f};
    float groinCy{0.5f};
    float groinScore{0.0f}; // 0 = none detected this run
};

namespace {
//! Axis-aligned box in normalized frame coords (center + size) carrying a detection score, used to
//! NMS the breast candidates down to distinct left/right breasts.
struct NBox
{
    float cx, cy, w, h, score;
};
} // namespace

NudeNet::NudeNet()
    : m_impl(std::make_unique<Impl>())
{
}

NudeNet::~NudeNet() = default;

bool NudeNet::IsSupported()
{
    return true;
}

bool NudeNet::Load(const std::string& modelPath, int size)
{
    if (modelPath.empty())
    {
        return false;
    }

    int reqSize = EnvInt("PROJECTM_NUDENET_SIZE", size);
    if (reqSize <= 0)
    {
        reqSize = 320;
    }
    reqSize = ((reqSize + 31) / 32) * 32; // snap to /32 for the conv stride

    try
    {
        bool useCuda = false;
        int cudaDevice = 0;
        const onnxcommon::EpConfig ep{"NudeNet", "PROJECTM_NUDENET_COREML", "PROJECTM_NUDENET_CUDA",
                                      "PROJECTM_NUDENET_CUDA_DEVICE"};
        Ort::SessionOptions options = onnxcommon::MakeSessionOptions(modelPath, ep, useCuda, cudaDevice);
        m_impl->session = std::make_unique<Ort::Session>(m_impl->env, modelPath.c_str(), options);
        m_impl->useCuda = useCuda;
        m_impl->cudaDevice = cudaDevice;
    }
    catch (const std::exception& e)
    {
        SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION, "[NudeNet] Failed to load %s: %s",
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

    SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, "[NudeNet] Loaded %s (input %dx%d, %zu outputs).",
                modelPath.c_str(), inW, inH, m_impl->outNames.size());
    return true;
}

bool NudeNet::IsLoaded() const
{
    return m_impl->session != nullptr;
}

void NudeNet::ProcessRgb(const uint8_t* rgb, int w, int h,
                         float boxX0, float boxY0, float boxX1, float boxY1, bool personValid)
{
    // Tunables (env-live): detection threshold, main-figure gate margin, and the flag/gender
    // temporal shaping. A flag latches on after onN consecutive detected runs, off after offN
    // misses; at ~2-3 Hz that is ~1 s each way. genderAlpha smooths the face gender axis.
    const float conf = EnvFloat("PROJECTM_NUDENET_CONF", 0.25f);
    const float margin = EnvFloat("PROJECTM_NUDENET_GATE_MARGIN", 0.12f);
    const int onN = EnvInt("PROJECTM_NUDENET_ON_N", 2);
    const int offN = EnvInt("PROJECTM_NUDENET_OFF_N", 3);
    const float glide = EnvFloat("PROJECTM_NUDENET_GLIDE", 0.4f);
    const float genderAlpha = EnvFloat("PROJECTM_NUDENET_EMA", 0.45f);
    // Separate, lower threshold for breast LOCATION boxes: covered breasts score modestly, and
    // location refinement should keep working even when the (higher) exposure cutoff rejects them.
    const float breastConf = EnvFloat("PROJECTM_NUDENET_BREAST_CONF", 0.3f);

    // Max gated detector score this run, per exposed class (0 = none / no main figure).
    float rawTop = 0.0f, rawRear = 0.0f, rawFrontF = 0.0f, rawFrontM = 0.0f;
    float rawFaceF = 0.0f, rawFaceM = 0.0f;
    std::vector<NBox> breastCand; // exposed breast boxes (normalized) for location refinement
    m_impl->breastN = 0;          // per-run; stays 0 on no-person / run failure
    m_impl->groinScore = 0.0f;    // per-run; best gated exposed-genitalia box, 0 = none

    if (m_impl->session && rgb != nullptr && w > 0 && h > 0 && personValid)
    {
        const int inW = m_impl->inW;
        const int inH = m_impl->inH;
        const int msize = std::min(inW, inH); // NudeNet is square
        if (static_cast<int>(m_impl->inputBuf.size()) < 3 * msize * msize)
        {
            m_impl->inputBuf.assign(static_cast<size_t>(3) * msize * msize, 0.0f);
        }
        float lbScale = 1.0f;
        int padX = 0, padY = 0;
        onnxcommon::RgbToChwLetterbox(rgb, w, h, msize, m_impl->inputBuf.data(), lbScale, padX, padY);

        Ort::MemoryInfo memInfo = Ort::MemoryInfo::CreateCpu(OrtArenaAllocator, OrtMemTypeDefault);
        const std::array<int64_t, 4> srcShape{1, 3, msize, msize};
        Ort::Value srcTensor = Ort::Value::CreateTensor<float>(
            memInfo, m_impl->inputBuf.data(),
            static_cast<size_t>(3) * msize * msize, srcShape.data(), srcShape.size());

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
            SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION, "[NudeNet] Run failed: %s", e.what());
            return; // hold last verdict
        }

        // YOLOv8 detect head: one 3-D tensor [1, F, N] (or [1, N, F]). F = 4 bbox + numClasses,
        // class scores already sigmoid'd, boxes decoded to model pixels. No baked NMS.
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
        if (det)
        {
            int nf = 0, na = 0, strideF = 0, strideA = 0;
            if (detD1 <= detD2) { nf = detD1; na = detD2; strideF = na; strideA = 1; }
            else { nf = detD2; na = detD1; strideF = 1; strideA = nf; }
            const int numClasses = nf - 4;
            auto val = [&](int f, int a) { return det[f * strideF + a * strideA]; };

            // Model-pixel -> normalized frame coords (y bottom-up, matching the pose bbox).
            const float invW = (w > 0) ? 1.0f / static_cast<float>(w) : 0.0f;
            const float invH = (h > 0) ? 1.0f / static_cast<float>(h) : 0.0f;
            auto toNormX = [&](float mx) { return std::clamp(((mx - padX) / lbScale) * invW, 0.0f, 1.0f); };
            auto toNormY = [&](float my) { return std::clamp(1.0f - ((my - padY) / lbScale) * invH, 0.0f, 1.0f); };

            // Main-figure gate box, expanded by `margin` so a tight pose bbox still admits a
            // breast/hip detection that pokes just past its edge.
            const float lo = std::min(boxX0, boxX1) - margin;
            const float hi = std::max(boxX0, boxX1) + margin;
            const float bot = std::min(boxY0, boxY1) - margin;
            const float top = std::max(boxY0, boxY1) + margin;

            auto keepMax = [&](float& dst, float score) { if (score > dst) { dst = score; } };

            if (numClasses > kClsMaleGenitaliaExposed)
            {
                for (int a = 0; a < na; ++a)
                {
                    // Gate on the box center first (cheap) before touching class scores.
                    const float ncx = toNormX(val(0, a));
                    const float ncy = toNormY(val(1, a));
                    if (ncx < lo || ncx > hi || ncy < bot || ncy > top) { continue; }

                    const float sTop = val(4 + kClsFemaleBreastExposed, a);
                    const float sRear = val(4 + kClsButtocksExposed, a);
                    const float sFrF = val(4 + kClsFemaleGenitaliaExposed, a);
                    const float sFrM = val(4 + kClsMaleGenitaliaExposed, a);
                    const float sFcF = val(4 + kClsFaceFemale, a);
                    const float sFcM = val(4 + kClsFaceMale, a);
                    if (sTop >= conf) { keepMax(rawTop, sTop); }
                    if (sRear >= conf) { keepMax(rawRear, sRear); }
                    if (sFrF >= conf) { keepMax(rawFrontF, sFrF); }
                    if (sFrM >= conf) { keepMax(rawFrontM, sFrM); }
                    if (sFcF >= conf) { keepMax(rawFaceF, sFcF); }
                    if (sFcM >= conf) { keepMax(rawFaceM, sFcM); }

                    // Breast LOCATION: EXPOSED breasts only -- covered boxes wander and would drag
                    // the already-good geometry when clothed, so refinement applies only when nude.
                    if (sTop >= breastConf)
                    {
                        breastCand.push_back({ncx, ncy,
                                              (val(2, a) / lbScale) * invW,
                                              (val(3, a) / lbScale) * invH, sTop});
                    }

                    // Groin LOCATION: EXPOSED genitalia only (female or male). One point, so just
                    // keep the highest-scoring gated box center.
                    const float sGroin = std::max(sFrF, sFrM);
                    if (sGroin >= breastConf && sGroin > m_impl->groinScore)
                    {
                        m_impl->groinScore = sGroin;
                        m_impl->groinCx = ncx;
                        m_impl->groinCy = ncy;
                    }
                }
            }

            // NMS the breast candidates into up to 2 distinct breasts (highest score first).
            std::sort(breastCand.begin(), breastCand.end(),
                      [](const NBox& p, const NBox& q) { return p.score > q.score; });
            for (const auto& c : breastCand)
            {
                if (m_impl->breastN >= 2) { break; }
                bool overlaps = false;
                for (int k = 0; k < m_impl->breastN; ++k)
                {
                    if (onnxcommon::BoxIoU(c.cx, c.cy, c.w, c.h, m_impl->breastCx[k],
                                           m_impl->breastCy[k], c.w, c.h) > 0.3f)
                    {
                        overlaps = true;
                        break;
                    }
                }
                if (!overlaps)
                {
                    m_impl->breastCx[m_impl->breastN] = c.cx;
                    m_impl->breastCy[m_impl->breastN] = c.cy;
                    m_impl->breastScore[m_impl->breastN] = c.score;
                    ++m_impl->breastN;
                }
            }
        }
    }

    // Update the four exposure flags on DETECTED/not (a gated box cleared conf this run).
    m_impl->top.Update(rawTop > 0.0f, onN, offN, glide);
    m_impl->rear.Update(rawRear > 0.0f, onN, offN, glide);
    m_impl->frontF.Update(rawFrontF > 0.0f, onN, offN, glide);
    m_impl->frontM.Update(rawFrontM > 0.0f, onN, offN, glide);

    // Gender axis: blend toward female_conf / (female_conf + male_conf) when a face is present,
    // else drift slowly back to 0.5 (unknown). Magnitude-weighted, unlike the presence flags.
    const float faceStrength = std::max(rawFaceF, rawFaceM);
    if (faceStrength >= conf)
    {
        const float target = rawFaceF / (rawFaceF + rawFaceM + 1e-4f);
        m_impl->female += (target - m_impl->female) * genderAlpha;
    }
    else
    {
        m_impl->female += (0.5f - m_impl->female) * (genderAlpha * 0.25f);
    }
}

float NudeNet::Top() const { return m_impl->top.out; }
float NudeNet::Rear() const { return m_impl->rear.out; }
float NudeNet::FrontF() const { return m_impl->frontF.out; }
float NudeNet::FrontM() const { return m_impl->frontM.out; }
float NudeNet::Female() const { return m_impl->female; }

int NudeNet::BreastCount() const { return m_impl->breastN; }

bool NudeNet::Breast(int index, float& x, float& y, float& score) const
{
    if (index < 0 || index >= m_impl->breastN) { return false; }
    x = m_impl->breastCx[index];
    y = m_impl->breastCy[index];
    score = m_impl->breastScore[index];
    return true;
}

bool NudeNet::GroinBox(float& x, float& y, float& score) const
{
    if (m_impl->groinScore <= 0.0f) { return false; }
    x = m_impl->groinCx;
    y = m_impl->groinCy;
    score = m_impl->groinScore;
    return true;
}
