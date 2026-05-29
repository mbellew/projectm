/**
 * @file videoCapture_macos.mm
 * @brief macOS AVFoundation camera capture backend.
 */
#include "videoCapture.hpp"

#import <AVFoundation/AVFoundation.h>
#import <CoreMedia/CoreMedia.h>
#import <CoreVideo/CoreVideo.h>

#include <cstdint>
#include <cstring>
#include <utility>
#include <vector>

@interface ProjectMVideoCaptureDelegate : NSObject <AVCaptureVideoDataOutputSampleBufferDelegate>
{
    VideoCapture::FrameCallback _callback;
    std::vector<uint8_t> _contiguousBuffer;
}
- (void)setCallback:(VideoCapture::FrameCallback)callback;
- (void)clearCallback;
@end

@implementation ProjectMVideoCaptureDelegate

- (void)setCallback:(VideoCapture::FrameCallback)callback
{
    _callback = std::move(callback);
}

- (void)clearCallback
{
    _callback = nullptr;
}

- (void)captureOutput:(AVCaptureOutput*)output
        didOutputSampleBuffer:(CMSampleBufferRef)sampleBuffer
        fromConnection:(AVCaptureConnection*)connection
{
    if (!_callback)
    {
        return;
    }

    CVImageBufferRef pixelBuffer = CMSampleBufferGetImageBuffer(sampleBuffer);
    if (!pixelBuffer)
    {
        return;
    }

    CVPixelBufferLockBaseAddress(pixelBuffer, kCVPixelBufferLock_ReadOnly);

    const int width = static_cast<int>(CVPixelBufferGetWidth(pixelBuffer));
    const int height = static_cast<int>(CVPixelBufferGetHeight(pixelBuffer));
    const size_t bytesPerRow = CVPixelBufferGetBytesPerRow(pixelBuffer);
    void* base = CVPixelBufferGetBaseAddress(pixelBuffer);

    const size_t expectedRow = static_cast<size_t>(width) * 4;
    if (bytesPerRow == expectedRow)
    {
        _callback(base, width, height, VideoCapture::PixelFormat::BGRA);
    }
    else
    {
        _contiguousBuffer.resize(expectedRow * height);
        const uint8_t* src = static_cast<const uint8_t*>(base);
        for (int y = 0; y < height; ++y)
        {
            std::memcpy(_contiguousBuffer.data() + y * expectedRow,
                        src + y * bytesPerRow,
                        expectedRow);
        }
        _callback(_contiguousBuffer.data(), width, height, VideoCapture::PixelFormat::BGRA);
    }

    CVPixelBufferUnlockBaseAddress(pixelBuffer, kCVPixelBufferLock_ReadOnly);
}

@end

struct VideoCapture::Impl
{
    AVCaptureSession* session{nil};
    ProjectMVideoCaptureDelegate* delegate{nil};
    dispatch_queue_t queue{nullptr};
    bool running{false};
};

VideoCapture::VideoCapture()
    : m_impl(std::make_unique<Impl>())
{
}

VideoCapture::~VideoCapture()
{
    Stop();
}

bool VideoCapture::Start(FrameCallback callback, const std::string& deviceNameSubstring)
{
    if (m_impl->running)
    {
        return false;
    }

    // Request user permission. Synchronously wait for the response so the caller knows
    // whether capture actually started.
    __block bool granted = false;
    dispatch_semaphore_t sema = dispatch_semaphore_create(0);
    [AVCaptureDevice requestAccessForMediaType:AVMediaTypeVideo
                             completionHandler:^(BOOL allowed) {
                                 granted = allowed;
                                 dispatch_semaphore_signal(sema);
                             }];
    dispatch_semaphore_wait(sema, dispatch_time(DISPATCH_TIME_NOW, 30LL * NSEC_PER_SEC));

    if (!granted)
    {
        return false;
    }

    // Trim surrounding whitespace and treat a blank value as "unspecified" so that
    // e.g. PROJECTM_VIDEO_DEVICE="" (or whitespace/newline) falls back to the default.
    std::string trimmedName;
    const auto firstNonSpace = deviceNameSubstring.find_first_not_of(" \t\r\n");
    if (firstNonSpace != std::string::npos)
    {
        const auto lastNonSpace = deviceNameSubstring.find_last_not_of(" \t\r\n");
        trimmedName = deviceNameSubstring.substr(firstNonSpace, lastNonSpace - firstNonSpace + 1);
    }

    AVCaptureDevice* device = nil;
    if (!trimmedName.empty())
    {
        // Enumerate all video-capable devices including virtual cameras (OBS, etc.).
        // externalUnknown covers DAL plug-ins like OBS Virtual Camera; on macOS 14+
        // it's preferred over the deprecated AVCaptureDevice +devicesWithMediaType:.
        NSArray<AVCaptureDeviceType>* deviceTypes = @[
            AVCaptureDeviceTypeBuiltInWideAngleCamera,
#if defined(MAC_OS_VERSION_14_0) && MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_VERSION_14_0
            AVCaptureDeviceTypeExternal,
#else
            AVCaptureDeviceTypeExternalUnknown,
#endif
        ];
        AVCaptureDeviceDiscoverySession* discovery =
            [AVCaptureDeviceDiscoverySession discoverySessionWithDeviceTypes:deviceTypes
                                                                   mediaType:AVMediaTypeVideo
                                                                    position:AVCaptureDevicePositionUnspecified];

        NSString* needle = [[NSString stringWithUTF8String:trimmedName.c_str()] lowercaseString];
        NSMutableArray<NSString*>* names = [NSMutableArray array];
        for (AVCaptureDevice* d in discovery.devices)
        {
            [names addObject:d.localizedName];
            if (!device && [[d.localizedName lowercaseString] containsString:needle])
            {
                device = d;
            }
        }
        if (!device)
        {
            // No match: warn and fall back to the system default rather than failing.
            NSLog(@"[VideoCapture] No device matches \"%s\"; falling back to default. Available: %@",
                  trimmedName.c_str(), names);
            device = [AVCaptureDevice defaultDeviceWithMediaType:AVMediaTypeVideo];
        }
        else
        {
            NSLog(@"[VideoCapture] Selected video device: %@", device.localizedName);
        }
    }
    else
    {
        device = [AVCaptureDevice defaultDeviceWithMediaType:AVMediaTypeVideo];
    }

    if (!device)
    {
        return false;
    }

    NSError* error = nil;
    AVCaptureDeviceInput* input = [AVCaptureDeviceInput deviceInputWithDevice:device error:&error];
    if (error || !input)
    {
        return false;
    }

    m_impl->session = [[AVCaptureSession alloc] init];
    [m_impl->session beginConfiguration];
    if ([m_impl->session canSetSessionPreset:AVCaptureSessionPreset640x480])
    {
        m_impl->session.sessionPreset = AVCaptureSessionPreset640x480;
    }
    if (![m_impl->session canAddInput:input])
    {
        [m_impl->session commitConfiguration];
        m_impl->session = nil;
        return false;
    }
    [m_impl->session addInput:input];

    AVCaptureVideoDataOutput* output = [[AVCaptureVideoDataOutput alloc] init];
    output.alwaysDiscardsLateVideoFrames = YES;
    output.videoSettings = @{(NSString*) kCVPixelBufferPixelFormatTypeKey: @(kCVPixelFormatType_32BGRA)};

    m_impl->delegate = [[ProjectMVideoCaptureDelegate alloc] init];
    [m_impl->delegate setCallback:std::move(callback)];

    m_impl->queue = dispatch_queue_create("net.projectm.video_capture", DISPATCH_QUEUE_SERIAL);
    [output setSampleBufferDelegate:m_impl->delegate queue:m_impl->queue];

    if (![m_impl->session canAddOutput:output])
    {
        [m_impl->session commitConfiguration];
        m_impl->session = nil;
        m_impl->delegate = nil;
        m_impl->queue = nullptr;
        return false;
    }
    [m_impl->session addOutput:output];
    [m_impl->session commitConfiguration];

    [m_impl->session startRunning];
    m_impl->running = true;
    return true;
}

void VideoCapture::Stop()
{
    if (!m_impl->running)
    {
        return;
    }
    [m_impl->session stopRunning];
    [m_impl->delegate clearCallback];
    m_impl->session = nil;
    m_impl->delegate = nil;
    m_impl->queue = nullptr;
    m_impl->running = false;
}

bool VideoCapture::IsRunning() const
{
    return m_impl->running;
}
