/**
 * @file poseTracker_stub.cpp
 * @brief No-op PoseTracker used when onnxruntime is unavailable (ENABLE_ONNX_SEG off).
 *
 * Same symbols as the real backend so pmSDL can reference PoseTracker
 * unconditionally; IsSupported()/Load() report unavailable.
 */
#include "poseTracker.hpp"

struct PoseTracker::Impl
{
};

PoseTracker::PoseTracker()
    : m_impl(nullptr)
{
}

PoseTracker::~PoseTracker() = default;

bool PoseTracker::IsSupported()
{
    return false;
}

bool PoseTracker::Load(const std::string& /*modelPath*/, int /*size*/)
{
    return false;
}

bool PoseTracker::IsLoaded() const
{
    return false;
}

void PoseTracker::Process(const uint8_t* /*bgra*/, int /*w*/, int /*h*/, bool /*mirror*/,
                          std::vector<PersonPose>& out)
{
    out.clear();
}
