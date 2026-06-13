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

bool SegMasker::IsLoaded() const
{
    return false;
}

void SegMasker::Process(const uint8_t* /*bgra*/, int /*w*/, int /*h*/, bool /*mirror*/,
                        std::vector<uint8_t>& /*outRGBA*/)
{
}
