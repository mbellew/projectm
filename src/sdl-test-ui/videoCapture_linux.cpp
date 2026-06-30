/**
 * @file videoCapture_linux.cpp
 * @brief Linux V4L2 camera capture backend.
 *
 * Mirrors the macOS AVFoundation backend's contract: enumerate capture devices,
 * pick one by preference-ordered case-insensitive name substrings (falling back
 * to the first usable device), stream frames at ~640x480 and deliver each as a
 * tightly-packed BGRX buffer on a worker thread. The alpha byte is meaningless
 * for a camera source, so frames are reported as BGRX (opaque) — otherwise
 * video_alpha_mode=0 (Source) would pick up junk alpha and render transparent.
 *
 * Stop() joins the worker thread, which drains any in-flight frame callback
 * (possibly deep in a SegMasker ONNX Run()) before returning, so the caller can
 * safely tear down the masker/handle afterwards.
 *
 * Pixel formats handled: YUYV (the near-universal UVC default), RGB24/BGR24
 * (common from v4l2loopback virtual cameras), and MJPEG (decoded via the vendored
 * stb_image) for cameras that only offer compressed output at the desired size.
 */
#include "videoCapture.hpp"

#include <stb_image.h>

#include <linux/videodev2.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <poll.h>
#include <unistd.h>

#include <algorithm>
#include <atomic>
#include <cctype>
#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <string>
#include <thread>
#include <vector>

namespace {

// ioctl wrapper that retries on EINTR (signals can interrupt blocking V4L2 ioctls).
int xioctl(int fd, unsigned long request, void* arg)
{
    int r;
    do
    {
        r = ioctl(fd, request, arg);
    } while (r == -1 && errno == EINTR);
    return r;
}

std::string toLower(std::string s)
{
    for (char& c : s) { c = static_cast<char>(std::tolower(static_cast<unsigned char>(c))); }
    return s;
}

std::string trim(const std::string& raw)
{
    const auto first = raw.find_first_not_of(" \t\r\n");
    if (first == std::string::npos) { return {}; }
    const auto last = raw.find_last_not_of(" \t\r\n");
    return raw.substr(first, last - first + 1);
}

struct Device
{
    std::string path;
    std::string card;
};

// Enumerate /dev/video* nodes that support single-planar video capture + streaming.
std::vector<Device> enumerateDevices()
{
    std::vector<Device> devices;
    for (int i = 0; i < 64; ++i)
    {
        std::string path = "/dev/video" + std::to_string(i);
        int fd = open(path.c_str(), O_RDWR | O_NONBLOCK);
        if (fd < 0) { continue; }

        v4l2_capability cap{};
        if (xioctl(fd, VIDIOC_QUERYCAP, &cap) == 0)
        {
            // A device's overall caps live in device_caps when V4L2_CAP_DEVICE_CAPS is set.
            const uint32_t caps = (cap.capabilities & V4L2_CAP_DEVICE_CAPS)
                                      ? cap.device_caps
                                      : cap.capabilities;
            if ((caps & V4L2_CAP_VIDEO_CAPTURE) && (caps & V4L2_CAP_STREAMING))
            {
                devices.push_back({path, reinterpret_cast<const char*>(cap.card)});
            }
        }
        close(fd);
    }
    return devices;
}

// Clamp an int to a uint8_t (saturating).
inline uint8_t clampU8(int v)
{
    return static_cast<uint8_t>(v < 0 ? 0 : (v > 255 ? 255 : v));
}

// Convert one YUYV macropixel pair (Y0 U Y1 V) to two BGRX pixels (BT.601).
inline void yuv2bgrx(int y, int u, int v, uint8_t* out)
{
    const int c = y - 16;
    const int d = u - 128;
    const int e = v - 128;
    out[0] = clampU8((298 * c + 516 * d + 128) >> 8);            // B
    out[1] = clampU8((298 * c - 100 * d - 208 * e + 128) >> 8);  // G
    out[2] = clampU8((298 * c + 409 * e + 128) >> 8);            // R
    out[3] = 255;                                                 // X (opaque)
}

// Pixel formats we can turn into BGRX (see the frame-delivery path). MJPEG is decoded via stb.
bool decodableFormat(uint32_t f)
{
    return f == V4L2_PIX_FMT_YUYV || f == V4L2_PIX_FMT_RGB24 ||
           f == V4L2_PIX_FMT_BGR24 || f == V4L2_PIX_FMT_MJPEG;
}

// A concrete capture mode the driver advertises.
struct CaptureFormat
{
    uint32_t pixelFormat{0};
    int width{0};
    int height{0};
    double maxFps{0.0}; // best frame rate the driver offers at this (format,size)
};

// Highest frame rate available for a (format,size) via VIDIOC_ENUM_FRAMEINTERVALS. For
// stepwise/continuous cameras the smallest interval (= max fps) is reported first.
double maxFpsFor(int fd, uint32_t pf, int w, int h)
{
    v4l2_frmivalenum fi{};
    fi.pixel_format = pf;
    fi.width = static_cast<uint32_t>(w);
    fi.height = static_cast<uint32_t>(h);
    double best = 0.0;
    for (fi.index = 0; xioctl(fd, VIDIOC_ENUM_FRAMEINTERVALS, &fi) == 0; ++fi.index)
    {
        if (fi.type == V4L2_FRMIVAL_TYPE_DISCRETE)
        {
            if (fi.discrete.numerator > 0)
            {
                best = std::max(best, static_cast<double>(fi.discrete.denominator) /
                                          static_cast<double>(fi.discrete.numerator));
            }
        }
        else // stepwise / continuous: min interval = max fps
        {
            if (fi.stepwise.min.numerator > 0)
            {
                best = std::max(best, static_cast<double>(fi.stepwise.min.denominator) /
                                          static_cast<double>(fi.stepwise.min.numerator));
            }
            break;
        }
    }
    return best;
}

// Enumerate every decodable (format, size) the device offers, with its max fps.
std::vector<CaptureFormat> enumerateFormats(int fd)
{
    std::vector<CaptureFormat> out;
    v4l2_fmtdesc fmt{};
    fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    for (fmt.index = 0; xioctl(fd, VIDIOC_ENUM_FMT, &fmt) == 0; ++fmt.index)
    {
        if (!decodableFormat(fmt.pixelformat)) { continue; }
        v4l2_frmsizeenum fs{};
        fs.pixel_format = fmt.pixelformat;
        for (fs.index = 0; xioctl(fd, VIDIOC_ENUM_FRAMESIZES, &fs) == 0; ++fs.index)
        {
            int w = 0, h = 0;
            if (fs.type == V4L2_FRMSIZE_TYPE_DISCRETE)
            {
                w = static_cast<int>(fs.discrete.width);
                h = static_cast<int>(fs.discrete.height);
            }
            else // stepwise/continuous: the maximum advertised size is the useful candidate
            {
                w = static_cast<int>(fs.stepwise.max_width);
                h = static_cast<int>(fs.stepwise.max_height);
            }
            if (w > 0 && h > 0)
            {
                out.push_back({fmt.pixelformat, w, h, maxFpsFor(fd, fmt.pixelformat, w, h)});
            }
            if (fs.type != V4L2_FRMSIZE_TYPE_DISCRETE) { break; }
        }
    }
    return out;
}

// True if this is an uncompressed layout (cheaper to decode than MJPEG); a tie-breaker.
bool isUncompressed(uint32_t f) { return f != V4L2_PIX_FMT_MJPEG; }

// Choose the best mode for our policy: FPS-first (hit targetFps as a hard floor), then the
// aspect closest to the display, then the highest resolution, then uncompressed over MJPEG.
// If nothing sustains targetFps, fall back to the fastest available (still max-area within that).
// displayAspect <= 0 disables the aspect preference. Returns false if the device offers nothing.
bool selectCaptureFormat(int fd, double targetFps, double displayAspect, CaptureFormat& out)
{
    std::vector<CaptureFormat> all = enumerateFormats(fd);
    if (all.empty()) { return false; }

    const double fpsFloor = (targetFps > 0.0) ? targetFps - 0.5 : 0.0;
    auto meetsFps = [&](const CaptureFormat& c) { return c.maxFps + 1e-6 >= fpsFloor; };
    const bool anyMeets = std::any_of(all.begin(), all.end(), meetsFps);

    auto aspectDist = [&](const CaptureFormat& c) {
        if (displayAspect <= 0.0 || c.height <= 0) { return 0.0; }
        return std::fabs(static_cast<double>(c.width) / c.height - displayAspect);
    };

    auto better = [&](const CaptureFormat& a, const CaptureFormat& b) {
        // 1. FPS-first: only when some mode meets the floor, meeting it beats not meeting it.
        if (anyMeets && meetsFps(a) != meetsFps(b)) { return meetsFps(a); }
        // 2. If no mode meets the floor, prefer the higher frame rate (get as close as we can).
        if (!anyMeets && std::fabs(a.maxFps - b.maxFps) > 0.5) { return a.maxFps > b.maxFps; }
        // 3. Aspect closest to the display (bucketed so near-equal aspects defer to resolution).
        const double da = aspectDist(a), db = aspectDist(b);
        if (std::fabs(da - db) > 0.05) { return da < db; }
        // 4. Highest resolution.
        const long areaA = static_cast<long>(a.width) * a.height;
        const long areaB = static_cast<long>(b.width) * b.height;
        if (areaA != areaB) { return areaA > areaB; }
        // 5. Uncompressed (YUYV/RGB/BGR) over MJPEG at equal size.
        if (isUncompressed(a.pixelFormat) != isUncompressed(b.pixelFormat))
        {
            return isUncompressed(a.pixelFormat);
        }
        return false;
    };

    out = *std::min_element(all.begin(), all.end(),
                            [&](const CaptureFormat& a, const CaptureFormat& b) { return better(a, b); });
    return true;
}

} // namespace

struct VideoCapture::Impl
{
    int fd{-1};
    std::thread worker;
    std::atomic<bool> running{false};
    FrameCallback callback;

    struct MappedBuffer
    {
        void* start{nullptr};
        size_t length{0};
    };
    std::vector<MappedBuffer> buffers;

    int width{0};
    int height{0};
    uint32_t pixelFormat{0};
    std::vector<uint8_t> bgrx; // contiguous BGRX output scratch

    void unmap()
    {
        for (auto& b : buffers)
        {
            if (b.start && b.start != MAP_FAILED) { munmap(b.start, b.length); }
        }
        buffers.clear();
    }
};

VideoCapture::VideoCapture()
    : m_impl(std::make_unique<Impl>())
{
}

VideoCapture::~VideoCapture()
{
    Stop();
}

bool VideoCapture::Start(FrameCallback callback, const std::vector<std::string>& preferredNameSubstrings,
                         double targetFps, double displayAspect)
{
    if (m_impl->running)
    {
        return false;
    }

    std::vector<Device> devices = enumerateDevices();
    if (devices.empty())
    {
        std::fprintf(stderr, "[VideoCapture] No V4L2 capture device found.\n");
        return false;
    }

    // Trim/​drop blank preferences so PROJECTM_VIDEO_DEVICE="" doesn't shadow the config list.
    std::vector<std::string> prefs;
    for (const auto& raw : preferredNameSubstrings)
    {
        std::string t = trim(raw);
        if (!t.empty()) { prefs.push_back(t); }
    }

    const Device* chosen = nullptr;
    for (const auto& pref : prefs)
    {
        const std::string needle = toLower(pref);
        for (const auto& d : devices)
        {
            if (toLower(d.card).find(needle) != std::string::npos)
            {
                chosen = &d;
                break;
            }
        }
        if (chosen)
        {
            std::fprintf(stderr, "[VideoCapture] Selected video device: %s (%s, matched \"%s\")\n",
                         chosen->card.c_str(), chosen->path.c_str(), pref.c_str());
            break;
        }
    }
    if (!chosen)
    {
        if (!prefs.empty())
        {
            std::fprintf(stderr, "[VideoCapture] No device matches preferences; using default %s (%s).\n",
                         devices.front().card.c_str(), devices.front().path.c_str());
        }
        chosen = &devices.front();
    }

    int fd = open(chosen->path.c_str(), O_RDWR | O_NONBLOCK);
    if (fd < 0)
    {
        std::fprintf(stderr, "[VideoCapture] Failed to open %s: %s\n", chosen->path.c_str(), std::strerror(errno));
        return false;
    }

    // Pick the capture mode: highest resolution that sustains targetFps (FPS-first), with the
    // aspect closest to the display preferred. Falls back to a plain 640x480 YUYV request if the
    // device advertises nothing we can enumerate/decode.
    v4l2_format fmt{};
    fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    fmt.fmt.pix.field = V4L2_FIELD_NONE;

    CaptureFormat chosenFmt{};
    if (selectCaptureFormat(fd, targetFps, displayAspect, chosenFmt))
    {
        fmt.fmt.pix.width = static_cast<uint32_t>(chosenFmt.width);
        fmt.fmt.pix.height = static_cast<uint32_t>(chosenFmt.height);
        fmt.fmt.pix.pixelformat = chosenFmt.pixelFormat;
        const char* cc = reinterpret_cast<const char*>(&chosenFmt.pixelFormat);
        std::fprintf(stderr,
                     "[VideoCapture] Selected mode %dx%d @%.0ffps '%c%c%c%c' (target %.0ffps, display aspect %.3f).\n",
                     chosenFmt.width, chosenFmt.height, chosenFmt.maxFps,
                     cc[0], cc[1], cc[2], cc[3], targetFps, displayAspect);
    }
    else
    {
        fmt.fmt.pix.width = 640;
        fmt.fmt.pix.height = 480;
        fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_YUYV;
    }

    if (xioctl(fd, VIDIOC_S_FMT, &fmt) == -1)
    {
        std::fprintf(stderr, "[VideoCapture] VIDIOC_S_FMT failed on %s: %s\n", chosen->path.c_str(), std::strerror(errno));
        close(fd);
        return false;
    }

    // If the driver substituted a format we can't decode (e.g. an MJPEG-only cam
    // ignored the request and picked something else), explicitly ask for MJPEG.
    if (!decodableFormat(fmt.fmt.pix.pixelformat))
    {
        fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_MJPEG;
        xioctl(fd, VIDIOC_S_FMT, &fmt);
    }

    const uint32_t pf = fmt.fmt.pix.pixelformat;
    if (!decodableFormat(pf))
    {
        const char* fourcc = reinterpret_cast<const char*>(&fmt.fmt.pix.pixelformat);
        std::fprintf(stderr,
                     "[VideoCapture] %s offers unsupported pixel format '%c%c%c%c' (need YUYV/RGB24/BGR24/MJPEG).\n",
                     chosen->path.c_str(), fourcc[0], fourcc[1], fourcc[2], fourcc[3]);
        close(fd);
        return false;
    }

    // Ask the driver to run at the target rate (best-effort; it snaps to the nearest supported
    // interval). Matching the display FPS keeps one fresh history slice per rendered frame.
    if (targetFps > 0.0)
    {
        v4l2_streamparm parm{};
        parm.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        parm.parm.capture.timeperframe.numerator = 1;
        parm.parm.capture.timeperframe.denominator = static_cast<uint32_t>(std::lround(targetFps));
        xioctl(fd, VIDIOC_S_PARM, &parm); // ignore failure; not all drivers honor S_PARM
    }

    m_impl->width = static_cast<int>(fmt.fmt.pix.width);
    m_impl->height = static_cast<int>(fmt.fmt.pix.height);
    m_impl->pixelFormat = pf;

    // Request mmap'd streaming buffers.
    v4l2_requestbuffers req{};
    req.count = 4;
    req.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    req.memory = V4L2_MEMORY_MMAP;
    if (xioctl(fd, VIDIOC_REQBUFS, &req) == -1 || req.count < 2)
    {
        std::fprintf(stderr, "[VideoCapture] VIDIOC_REQBUFS failed on %s.\n", chosen->path.c_str());
        close(fd);
        return false;
    }

    m_impl->buffers.resize(req.count);
    for (uint32_t i = 0; i < req.count; ++i)
    {
        v4l2_buffer buf{};
        buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        buf.memory = V4L2_MEMORY_MMAP;
        buf.index = i;
        if (xioctl(fd, VIDIOC_QUERYBUF, &buf) == -1)
        {
            std::fprintf(stderr, "[VideoCapture] VIDIOC_QUERYBUF failed.\n");
            m_impl->unmap();
            close(fd);
            return false;
        }
        m_impl->buffers[i].length = buf.length;
        m_impl->buffers[i].start =
            mmap(nullptr, buf.length, PROT_READ | PROT_WRITE, MAP_SHARED, fd, buf.m.offset);
        if (m_impl->buffers[i].start == MAP_FAILED)
        {
            std::fprintf(stderr, "[VideoCapture] mmap failed.\n");
            m_impl->unmap();
            close(fd);
            return false;
        }
    }

    // Queue all buffers, then start streaming.
    for (uint32_t i = 0; i < req.count; ++i)
    {
        v4l2_buffer buf{};
        buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        buf.memory = V4L2_MEMORY_MMAP;
        buf.index = i;
        if (xioctl(fd, VIDIOC_QBUF, &buf) == -1)
        {
            std::fprintf(stderr, "[VideoCapture] VIDIOC_QBUF failed.\n");
            m_impl->unmap();
            close(fd);
            return false;
        }
    }

    v4l2_buf_type type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    if (xioctl(fd, VIDIOC_STREAMON, &type) == -1)
    {
        std::fprintf(stderr, "[VideoCapture] VIDIOC_STREAMON failed.\n");
        m_impl->unmap();
        close(fd);
        return false;
    }

    m_impl->fd = fd;
    m_impl->callback = std::move(callback);
    m_impl->bgrx.resize(static_cast<size_t>(m_impl->width) * m_impl->height * 4);
    m_impl->running = true;

    m_impl->worker = std::thread([this]() {
        Impl* impl = m_impl.get();
        const int w = impl->width;
        const int h = impl->height;

        while (impl->running.load(std::memory_order_acquire))
        {
            pollfd pfd{};
            pfd.fd = impl->fd;
            pfd.events = POLLIN;
            const int pr = poll(&pfd, 1, 200); // 200ms timeout so we notice Stop() promptly
            if (pr <= 0)
            {
                continue; // timeout or EINTR/error -> re-check running flag
            }

            v4l2_buffer buf{};
            buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
            buf.memory = V4L2_MEMORY_MMAP;
            if (xioctl(impl->fd, VIDIOC_DQBUF, &buf) == -1)
            {
                if (errno == EAGAIN) { continue; }
                break; // unrecoverable
            }

            const uint8_t* src = static_cast<const uint8_t*>(impl->buffers[buf.index].start);
            uint8_t* dst = impl->bgrx.data();
            int cbW = w;
            int cbH = h;

            if (impl->pixelFormat == V4L2_PIX_FMT_YUYV)
            {
                const int pairs = (w * h) / 2;
                for (int p = 0; p < pairs; ++p)
                {
                    const uint8_t* yuyv = src + p * 4;
                    const int y0 = yuyv[0], u = yuyv[1], y1 = yuyv[2], v = yuyv[3];
                    yuv2bgrx(y0, u, v, dst + p * 8);
                    yuv2bgrx(y1, u, v, dst + p * 8 + 4);
                }
            }
            else if (impl->pixelFormat == V4L2_PIX_FMT_RGB24)
            {
                for (int i = 0; i < w * h; ++i)
                {
                    dst[i * 4 + 0] = src[i * 3 + 2]; // B
                    dst[i * 4 + 1] = src[i * 3 + 1]; // G
                    dst[i * 4 + 2] = src[i * 3 + 0]; // R
                    dst[i * 4 + 3] = 255;
                }
            }
            else if (impl->pixelFormat == V4L2_PIX_FMT_BGR24)
            {
                for (int i = 0; i < w * h; ++i)
                {
                    dst[i * 4 + 0] = src[i * 3 + 0]; // B
                    dst[i * 4 + 1] = src[i * 3 + 1]; // G
                    dst[i * 4 + 2] = src[i * 3 + 2]; // R
                    dst[i * 4 + 3] = 255;
                }
            }
            else // V4L2_PIX_FMT_MJPEG: decode the compressed frame (buf.bytesused) to RGB.
            {
                int dw = 0, dh = 0, channels = 0;
                stbi_uc* rgb = stbi_load_from_memory(src, static_cast<int>(buf.bytesused),
                                                     &dw, &dh, &channels, 3);
                if (!rgb)
                {
                    // Corrupt/partial JPEG — skip this frame and keep streaming.
                    if (xioctl(impl->fd, VIDIOC_QBUF, &buf) == -1) { break; }
                    continue;
                }
                if (static_cast<size_t>(dw) * dh * 4 != impl->bgrx.size())
                {
                    impl->bgrx.resize(static_cast<size_t>(dw) * dh * 4);
                    dst = impl->bgrx.data();
                }
                for (int i = 0; i < dw * dh; ++i)
                {
                    dst[i * 4 + 0] = rgb[i * 3 + 2]; // B
                    dst[i * 4 + 1] = rgb[i * 3 + 1]; // G
                    dst[i * 4 + 2] = rgb[i * 3 + 0]; // R
                    dst[i * 4 + 3] = 255;
                }
                stbi_image_free(rgb);
                cbW = dw;
                cbH = dh;
            }

            if (impl->callback)
            {
                impl->callback(dst, cbW, cbH, VideoCapture::PixelFormat::BGRX);
            }

            if (xioctl(impl->fd, VIDIOC_QBUF, &buf) == -1)
            {
                break; // failed to requeue -> stop
            }
        }
    });

    return true;
}

void VideoCapture::Stop()
{
    if (!m_impl->running)
    {
        return;
    }

    // Signal the worker to exit, then join. Joining blocks until any in-flight
    // callback (which may be deep in a SegMasker ONNX Run()) finishes, so the
    // caller can safely free the masker/handle right after Stop().
    m_impl->running = false;
    if (m_impl->worker.joinable())
    {
        m_impl->worker.join();
    }

    if (m_impl->fd >= 0)
    {
        v4l2_buf_type type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        xioctl(m_impl->fd, VIDIOC_STREAMOFF, &type);
    }
    m_impl->unmap();
    if (m_impl->fd >= 0)
    {
        close(m_impl->fd);
        m_impl->fd = -1;
    }
    m_impl->callback = nullptr;
}

bool VideoCapture::IsRunning() const
{
    return m_impl->running;
}
