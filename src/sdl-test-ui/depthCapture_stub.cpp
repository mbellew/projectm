/**
 * @file depthCapture_stub.cpp
 * @brief No-op DepthCapture used when depthai-core is unavailable (ENABLE_LUXONIS off).
 *
 * Provides the same symbols as the real OAK backend so pmSDL can reference
 * DepthCapture unconditionally; Start() simply reports no device.
 */
#include "depthCapture.hpp"

struct DepthCapture::Impl
{
};

DepthCapture::DepthCapture()
    : m_impl(nullptr)
{
}

DepthCapture::~DepthCapture() = default;

bool DepthCapture::IsSupported()
{
    return false;
}

bool DepthCapture::Start(FrameCallback /*callback*/, int /*width*/, int /*height*/)
{
    return false;
}

void DepthCapture::Stop()
{
}

bool DepthCapture::IsRunning() const
{
    return false;
}
