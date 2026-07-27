/**
 * @file nudeNet_stub.cpp
 * @brief No-op NudeNet used when onnxruntime is unavailable (ENABLE_ONNX_SEG off).
 *
 * Same symbols as the real backend so pmSDL can reference NudeNet unconditionally;
 * IsSupported()/Load() report unavailable and every scalar reads its covered default.
 */
#include "nudeNet.hpp"

struct NudeNet::Impl
{
};

NudeNet::NudeNet()
    : m_impl(nullptr)
{
}

NudeNet::~NudeNet() = default;

bool NudeNet::IsSupported()
{
    return false;
}

bool NudeNet::Load(const std::string& /*modelPath*/, int /*size*/)
{
    return false;
}

bool NudeNet::IsLoaded() const
{
    return false;
}

void NudeNet::ProcessRgb(const uint8_t* /*rgb*/, int /*w*/, int /*h*/,
                         float /*boxX0*/, float /*boxY0*/, float /*boxX1*/, float /*boxY1*/,
                         bool /*personValid*/)
{
}

float NudeNet::Top() const { return 0.0f; }
float NudeNet::Rear() const { return 0.0f; }
float NudeNet::FrontF() const { return 0.0f; }
float NudeNet::FrontM() const { return 0.0f; }
float NudeNet::Female() const { return 0.5f; }
int NudeNet::BreastCount() const { return 0; }
bool NudeNet::Breast(int /*index*/, float& /*x*/, float& /*y*/, float& /*score*/) const { return false; }
bool NudeNet::GroinBox(float& /*x*/, float& /*y*/, float& /*score*/) const { return false; }
