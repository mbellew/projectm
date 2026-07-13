/**
 * @file videoCapture.hpp
 * @brief Platform-agnostic camera capture for feeding projectm_video_submit_frame().
 */
#pragma once

#include <functional>
#include <memory>
#include <string>
#include <vector>

class VideoCapture
{
public:
    enum class PixelFormat
    {
        BGRA, //!< 4 bytes per pixel, channel order B, G, R, A.
        BGRX, //!< 4 bytes per pixel, channel order B, G, R, X; alpha byte is undefined (opaque source).
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
     * @param callback Per-frame callback invoked from a capture-backend thread.
     * @param preferredNameSubstrings Preference-ordered, case-insensitive substrings
     *        matched against device localized names (e.g. {"OBS", "FaceTime"}). The first
     *        enumerated device matching any entry, in list order, is chosen. An empty list
     *        (or no match) selects the system default video device.
     * @param targetFps Desired capture frame rate (typically the display's target FPS). The
     *        backend picks the highest-resolution mode that sustains this as a hard floor.
     *        <= 0 leaves the rate unconstrained (backend default).
     * @param displayAspect Output display aspect ratio (width/height, e.g. 16.0/9.0). When the
     *        camera offers multiple aspect ratios, the one closest to this is preferred so the
     *        captured frame matches the screen. <= 0 disables the aspect preference.
     * @return true if capture started; false if denied or unsupported.
     */
    bool Start(FrameCallback callback, const std::vector<std::string>& preferredNameSubstrings = {},
               double targetFps = 0.0, double displayAspect = 0.0);

    /**
     * Replays frames from disk instead of the camera ($PROJECTM_VIDEO_FILE; Linux only).
     * @param path A single image, or a directory of images played in filename order, looping.
     *
     * The callback contract is identical to the live path, so seg / depth gate / pose all behave as
     * they would on camera -- but the scene is IDENTICAL every run, which is what makes a change to
     * the subject gate judgeable. A live camera cannot hold a background figure still across builds.
     */
    bool StartFileReplay(const std::string& path, FrameCallback callback, double targetFps);

    /** Stops capture and releases the device. */
    void Stop();

    bool IsRunning() const;

private:
    struct Impl;
    std::unique_ptr<Impl> m_impl;
};
