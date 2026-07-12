/**
 * @file segMask_stub.cpp
 * @brief No-op SegMasker used when onnxruntime is unavailable (ENABLE_ONNX_SEG off).
 *
 * Same symbols as the real backend so pmSDL can reference SegMasker
 * unconditionally; IsSupported()/Load() report unavailable.
 */
#include "segMask.hpp"

struct SegMasker::Impl
{
};

SegMasker::SegMasker()
    : m_impl(nullptr)
{
}

SegMasker::~SegMasker() = default;

bool SegMasker::IsSupported()
{
    return false;
}

bool SegMasker::Load(const std::string& /*modelPath*/, int /*size*/, float /*downsampleRatio*/)
{
    return false;
}

bool SegMasker::LoadSecondary(const std::string& /*modelPath*/, int /*size*/, float /*downsampleRatio*/,
                              const std::string& /*combine*/, float /*gateThreshold*/)
{
    return false;
}

bool SegMasker::LoadDepth(const std::string& /*modelPath*/, int /*size*/, float /*band*/,
                          bool /*invert*/)
{
    return false;
}

bool SegMasker::IsLoaded() const
{
    return false;
}

const SegTimings& SegMasker::LastTimings() const
{
    static const SegTimings kNone{};
    return kNone;
}

const uint8_t* SegMasker::RgbFrame() const
{
    return nullptr;
}

bool SegMasker::HasDepth() const
{
    return false;
}

float SegMasker::SampleDepth(float /*fx*/, float /*fy*/) const
{
    return -1.0f;
}

void SegMasker::Process(const uint8_t* /*bgra*/, int /*w*/, int /*h*/, bool /*mirror*/,
                        std::vector<uint8_t>& /*outRGBA*/)
{
}

void SegMasker::ApplyDepthGate(int /*w*/, int /*h*/, std::vector<uint8_t>& /*outRGBA*/)
{
}

void SegMasker::HardenAlpha(int /*w*/, int /*h*/, std::vector<uint8_t>& /*outRGBA*/)
{
}

void SegMasker::SetHarden(float /*lo*/, float /*hi*/)
{
}
