/**
 * @file videoCapture.hpp
 * @brief Platform-agnostic camera capture for feeding projectm_video_submit_frame().
 */
#pragma once

#include <functional>
#include <memory>

class VideoCapture
{
public:
    enum class PixelFormat
    {
        BGRA, //!< 4 bytes per pixel, channel order B, G, R, A.
    };

    /**
     * Frame callback. Invoked from the capture backend's worker thread.
     * @param data Tightly-packed pixel data (rows = width * 4 bytes).
     * @param width Frame width in pixels.
     * @param height Frame height in pixels.
     * @param format Pixel layout.
     */
    using FrameCallback = std::function<void(const void* data, int width, int height, PixelFormat format)>;

    VideoCapture();
    ~VideoCapture();

    VideoCapture(const VideoCapture&) = delete;
    VideoCapture& operator=(const VideoCapture&) = delete;

    /**
     * Requests camera access and begins capture. Blocks briefly while the user
     * responds to the system permission prompt on first use.
     * @return true if capture started; false if denied or unsupported.
     */
    bool Start(FrameCallback callback);

    /** Stops capture and releases the device. */
    void Stop();

    bool IsRunning() const;

private:
    struct Impl;
    std::unique_ptr<Impl> m_impl;
};
