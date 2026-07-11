/**
* projectM -- Milkdrop-esque visualisation SDK
* Copyright (C)2003-2019 projectM Team
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
* See 'LICENSE.txt' included within this release
*
* projectM-sdl
* This is an implementation of projectM using libSDL2
*
* pmSDL.cpp
* Authors: Created by Mischa Spiegelmock on 2017-09-18.
*
*
* experimental Stereoscopic SBS driver functionality by
*	RobertPancoast77@gmail.com
*/

#include "pmSDL.hpp"

#include "screenshot.hpp"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

#ifdef PROJECTM_VIDEO_CAPTURE_ENABLED
#include <projectM-4/video.h>
#endif

namespace {
auto dispatchLoadProc(const char* name, void* userData) -> void*
{
    // Dispatch load proc to SDL
    return SDL_GL_GetProcAddress(name);
}

// Aspect ratio (w/h) of the physical fullscreen display. Camera capture is negotiated once at
// startup, so it targets the stable desktop aspect rather than the (resizable) window. Returns
// 0 when unknown, which disables the capture-side aspect preference.
auto desktopAspect() -> double
{
    SDL_DisplayMode dm;
    if (SDL_GetDesktopDisplayMode(0, &dm) == 0 && dm.h > 0)
    {
        return static_cast<double>(dm.w) / static_cast<double>(dm.h);
    }
    return 0.0;
}
} // namespace

projectMSDL::projectMSDL(SDL_GLContext glCtx, const std::string& presetPath)
    : _openGlContext(glCtx)
    , _projectM(projectm_create_with_opengl_load_proc(&dispatchLoadProc, nullptr))
    , _playlist(projectm_playlist_create(_projectM))
{
    projectm_get_window_size(_projectM, &_width, &_height);
    projectm_playlist_set_preset_switched_event_callback(_playlist, &projectMSDL::presetSwitchedEvent, static_cast<void*>(this));
    projectm_playlist_add_path(_playlist, presetPath.c_str(), true, false);
    // Directory scan order is filesystem-defined; sort by full path so playback is alphabetical
    // (folders grouped, names ascending). A PROJECTM_PRESET_LIST keeps its file order instead.
    projectm_playlist_sort(_playlist, 0, projectm_playlist_size(_playlist),
                           SORT_PREDICATE_FULL_PATH, SORT_ORDER_ASCENDING);
    projectm_playlist_set_shuffle(_playlist, _shuffle);
    dumpOpenGLInfo();
    enableGLDebugOutput();
}

projectMSDL::projectMSDL(SDL_GLContext glCtx, const std::vector<std::string>& presetList)
    : _openGlContext(glCtx)
    , _projectM(projectm_create_with_opengl_load_proc(&dispatchLoadProc, nullptr))
    , _playlist(projectm_playlist_create(_projectM))
{
    projectm_get_window_size(_projectM, &_width, &_height);
    projectm_playlist_set_preset_switched_event_callback(_playlist, &projectMSDL::presetSwitchedEvent, static_cast<void*>(this));
    for (const auto& preset : presetList)
    {
        projectm_playlist_add_preset(_playlist, preset.c_str(), true);
    }
    projectm_playlist_set_shuffle(_playlist, _shuffle);
    dumpOpenGLInfo();
    enableGLDebugOutput();
}

projectMSDL::~projectMSDL()
{
#ifdef PROJECTM_VIDEO_CAPTURE_ENABLED
    stopVideoCapture();
#endif
    projectm_playlist_destroy(_playlist);
    _playlist = nullptr;
    projectm_destroy(_projectM);
    _projectM = nullptr;
}

#ifdef PROJECTM_VIDEO_CAPTURE_ENABLED
void projectMSDL::startVideoCapture()
{
    if (_videoCapture && _videoCapture->IsRunning())
    {
        return;
    }
    if (!_videoCapture)
    {
        _videoCapture = std::make_unique<VideoCapture>();
    }

    // $PROJECTM_VIDEO_MASK / "Video Mask" selects the app-side mask PRODUCER that
    // fills the submitted frame's alpha channel with a clean foreground mask. It
    // does NOT force the library's alpha mode — the preset's video_alpha_mode
    // decides what to do with the supplied alpha (0/Source = use it as-is; 2/3 =
    // ignore it and synthesize motion/motion-decay from RGB; a shader can ignore
    // alpha entirely for full video). The library alpha mode stays preset-controlled.
    //   off (default) = submit raw video (opaque alpha)
    //   seg           = host ONNX person segmentation on the webcam color frame
    //   (an OAK/Luxonis video device additionally produces a depth mask, below)
    bool useSeg = false;
    {
        const char* maskEnv = std::getenv("PROJECTM_VIDEO_MASK");
        std::string m = maskEnv ? std::string(maskEnv) : _videoMaskPref; // env overrides config
        // Tolerate a legacy trailing "-raw" (the old refine toggle; refine is now
        // a library/preset concern, not an app one).
        if (m.size() > 4 && m.compare(m.size() - 4, 4, "-raw") == 0) { m.erase(m.size() - 4); }
        useSeg = (m == "seg" || m == "person");
    }

    auto* handle = _projectM;
    // Source preference order: $PROJECTM_VIDEO_DEVICE (if set) wins, then the config
    // "Video Devices" list, then the system default. Each entry is a case-insensitive
    // name substring (e.g. "OBS" for the OBS Virtual Camera).
    std::vector<std::string> preferredDevices;
    if (const char* envDev = std::getenv("PROJECTM_VIDEO_DEVICE"))
    {
        if (envDev[0])
        {
            preferredDevices.emplace_back(envDev);
        }
    }
    preferredDevices.insert(preferredDevices.end(), _videoDevicePrefs.begin(), _videoDevicePrefs.end());

    // Host ONNX person-segmentation path: run the matting model on the webcam
    // color frame. The matte comes from the same frame, so it is time-aligned to
    // the RGB (no depth-sensor lag). Submit the finished RGBA through Source +
    // refine. On any failure, fall through to the plain webcam path.
    if (useSeg)
    {
        if (!SegMasker::IsSupported())
        {
            SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION,
                        "Video Mask=seg requested but ONNX support is not built in "
                        "(configure with -DENABLE_ONNX_SEG=ON). Falling back to webcam.");
        }
        else
        {
            if (!_segMasker)
            {
                _segMasker = std::make_unique<SegMasker>();
            }
            // Model path: $PROJECTM_SEG_MODEL > "Video Seg Model" config > default.
            std::string modelPath;
            if (const char* env = std::getenv("PROJECTM_SEG_MODEL"); env && env[0])
            {
                modelPath = env;
            }
            else if (!_segModelPath.empty())
            {
                modelPath = _segModelPath;
            }
            else if (const char* home = std::getenv("HOME"))
            {
                modelPath = std::string(home) + "/.projectM/models/rvm_mobilenetv3.onnx";
            }

            // Quality level: $PROJECTM_SEG_QUALITY > "Video Seg Quality" config > 2.
            int quality = _segQuality;
            if (const char* q = std::getenv("PROJECTM_SEG_QUALITY"); q && q[0])
            {
                quality = std::atoi(q);
            }
            if (quality < 1 || quality > 3)
            {
                quality = 2;
            }
            const int segSize = (quality == 1) ? 256 : (quality == 3) ? 512 : 384;

            const bool loaded = _segMasker->IsLoaded() || _segMasker->Load(modelPath, segSize);

            // Optional second model: $PROJECTM_SEG_MODEL2 > "Video Seg Model 2". Its matte is
            // multiplied into the primary's (e.g. RVM soft matte x person mask = soft people-only).
            if (loaded)
            {
                std::string modelPath2;
                if (const char* env = std::getenv("PROJECTM_SEG_MODEL2"); env && env[0])
                {
                    modelPath2 = env;
                }
                else if (!_segModelPath2.empty())
                {
                    modelPath2 = _segModelPath2;
                }
                if (!modelPath2.empty())
                {
                    // Combine mode: $PROJECTM_SEG_COMBINE > "Video Seg Combine" config > multiply.
                    std::string combine = "multiply";
                    if (const char* env = std::getenv("PROJECTM_SEG_COMBINE"); env && env[0])
                    {
                        combine = env;
                    }
                    else if (!_segCombine.empty())
                    {
                        combine = _segCombine;
                    }
                    float gate = 0.5f;
                    if (const char* g = std::getenv("PROJECTM_SEG_GATE"); g && g[0])
                    {
                        gate = static_cast<float>(std::atof(g));
                    }
                    _segMasker->LoadSecondary(modelPath2, segSize, 0.0f, combine, gate);
                }

                // Optional monocular depth gate: $PROJECTM_SEG_DEPTH_MODEL > "Video Seg Depth
                // Model". When set, background people (spectators/passers-by) are dropped from the
                // matte by relative depth -- keeping everyone up front. Off when no model is given.
                std::string depthModel;
                if (const char* env = std::getenv("PROJECTM_SEG_DEPTH_MODEL"); env && env[0])
                {
                    depthModel = env;
                }
                else if (!_segDepthModel.empty())
                {
                    depthModel = _segDepthModel;
                }
                if (!depthModel.empty())
                {
                    _segMasker->LoadDepth(depthModel, 0, static_cast<float>(_segDepthBand), false);
                }

                // Matte-hardening smoothstep edges ("Video Seg Harden Lo/Hi"); env overrides apply
                // inside HardenAlpha. Off by default (lo=0, hi=1) -> raw matte unchanged.
                _segMasker->SetHarden(static_cast<float>(_segHardenLo),
                                      static_cast<float>(_segHardenHi));
            }

            if (!loaded)
            {
                SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION,
                            "Video Mask=seg: model '%s' failed to load; falling back to webcam.",
                            modelPath.c_str());
            }
            else
            {
                // Producer only: supply the matte in alpha; the preset's
                // video_alpha_mode (default 0/Source) decides whether to use it.
                auto* masker = _segMasker.get();
                auto outBuf = std::make_shared<std::vector<uint8_t>>();

                // Optional body-pose model for the pose->touch bridge: $PROJECTM_POSE_MODEL >
                // "Video Pose Model". When present, YOLO-pose runs on the same un-mirrored frame
                // as seg (shared coordinate space). Tracker-first: for now it only logs detections.
                PoseTracker* pose = nullptr;
                if (PoseTracker::IsSupported())
                {
                    std::string poseModel;
                    if (const char* env = std::getenv("PROJECTM_POSE_MODEL"); env && env[0])
                    {
                        poseModel = env;
                    }
                    else if (!_poseModelPath.empty())
                    {
                        poseModel = _poseModelPath;
                    }
                    if (!poseModel.empty())
                    {
                        if (!_poseTracker)
                        {
                            _poseTracker = std::make_unique<PoseTracker>();
                        }
                        if (_poseTracker->IsLoaded() || _poseTracker->Load(poseModel))
                        {
                            pose = _poseTracker.get();
                            if (!_poseBridge)
                            {
                                PoseTouchParams params;
                                params.refFps = static_cast<float>(_fps);
                                params.ReadEnvOverrides();
                                _poseBridge = std::make_unique<PoseTouchBridge>(params);
                            }
                            SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                                        "Body-pose tracking active (pose->touch bridge).");
                        }
                    }
                }

                // Pose->touch confidence tuning (env, live). confFloor: drop wrists below this raw
                // keypoint confidence. alphaFloor: how much a wrist OFF the seg matte keeps of its
                // confidence (1.0 = ignore the matte entirely). A fast-waving hand is often clipped
                // by the matte, so the alpha weighting must stay gentle or it erases the gesture.
                auto envF = [](const char* n, float d) {
                    const char* v = std::getenv(n);
                    return (v && v[0]) ? static_cast<float>(std::atof(v)) : d;
                };
                const float confFloor = envF("PROJECTM_POSE_CONF_FLOOR", 0.15f);
                // 0.7 = an off-matte wrist keeps 70% of its confidence. A fast wave is often clipped
                // by the seg matte, so this must stay high or the gesture we track gets erased.
                const float alphaFloor = envF("PROJECTM_POSE_ALPHA_FLOOR", 0.7f);

                // Library owns the mirror (projectm_video_set_mirror), so the matte
                // travels with the RGB either way — seg passes mirror=false.
                const bool segOk = _videoCapture->Start(
                    [handle, masker, outBuf, pose, this, confFloor, alphaFloor](
                        const void* data, int width, int height, VideoCapture::PixelFormat /*fmt*/) {
                        masker->Process(static_cast<const uint8_t*>(data), width, height,
                                        /*mirror=*/false, *outBuf);
                        projectm_video_submit_frame(handle, outBuf->data(),
                                                    static_cast<unsigned int>(width),
                                                    static_cast<unsigned int>(height),
                                                    PROJECTM_VIDEO_FORMAT_RGBA);

                        // Run body-pose first (if enabled): its torso keypoints give a better
                        // "center" than the matte centroid, and its wrists drive the touch bridge.
                        static std::vector<PersonPose> poses;
                        if (pose)
                        {
                            pose->Process(static_cast<const uint8_t*>(data), width, height,
                                          /*mirror=*/false, poses);
                        }
                        else
                        {
                            poses.clear();
                        }

                        // Alpha-weighted centroid of the matte (the fallback "center") -> seg_*.
                        // Row 0 is the top of the frame; flip Y so cy matches preset per-pixel
                        // y (bottom-up). The library applies mirror and all smoothing.
                        const uint8_t* px = outBuf->data();
                        double sumA = 0.0, sumXA = 0.0, sumYA = 0.0;
                        const double invW = (width > 1) ? 1.0 / (width - 1) : 0.0;
                        const double invH = (height > 1) ? 1.0 / (height - 1) : 0.0;
                        for (int row = 0; row < height; ++row)
                        {
                            const double yv = 1.0 - row * invH; // bottom-up
                            for (int col = 0; col < width; ++col)
                            {
                                const double a = px[(static_cast<size_t>(row) * width + col) * 4 + 3] / 255.0;
                                sumA += a;
                                sumXA += a * (col * invW);
                                sumYA += a * yv;
                            }
                        }
                        float cx = 0.5f, cy = 0.5f, coverage = 0.0f;
                        if (sumA > 1e-6)
                        {
                            cx = static_cast<float>(sumXA / sumA);
                            cy = static_cast<float>(sumYA / sumA);
                            coverage = static_cast<float>(sumA / (static_cast<double>(width) * height));
                        }

                        // Prefer a pose-derived chest/heart point when a confident torso is present:
                        // the matte centroid sits at the belly-button, whereas the shoulder midpoint
                        // dropped ~25% toward the hips is roughly the sternum. Un-mirrored, like the
                        // matte centroid (the library mirrors internally). Best (first) person only.
                        if (!poses.empty())
                        {
                            const PersonPose& p = poses.front();
                            const Keypoint& ls = p[Kpt::LeftShoulder];
                            const Keypoint& rs = p[Kpt::RightShoulder];
                            if (ls.conf > 0.4f && rs.conf > 0.4f)
                            {
                                float chestX = (ls.x + rs.x) * 0.5f;
                                float chestY = (ls.y + rs.y) * 0.5f;
                                const Keypoint& lh = p[Kpt::LeftHip];
                                const Keypoint& rh = p[Kpt::RightHip];
                                if (lh.conf > 0.3f && rh.conf > 0.3f)
                                {
                                    chestX += 0.25f * ((lh.x + rh.x) * 0.5f - chestX);
                                    chestY += 0.25f * ((lh.y + rh.y) * 0.5f - chestY);
                                }
                                else
                                {
                                    chestY -= 0.06f; // no hips: nudge just below the shoulder line
                                }
                                cx = std::clamp(chestX, 0.0f, 1.0f);
                                cy = std::clamp(chestY, 0.0f, 1.0f);
                            }
                        }
                        projectm_video_set_seg_centroid(handle, cx, cy, coverage);

                        // Turn each detected wrist into a HandObservation (seg-fused: matte alpha
                        // weights confidence, depth = wrist closeness) and stash for the main-thread
                        // bridge drain. projectm_touch* is main-thread-only, so we do NOT call it here.
                        if (pose)
                        {
                            const uint8_t* rgba = outBuf->data();
                            auto sampleAlpha = [&](float nx, float ny) -> float {
                                // nx left->right, ny bottom-up; RGBA row 0 is the top of the frame.
                                const int col = std::clamp(static_cast<int>(nx * (width - 1) + 0.5f), 0, width - 1);
                                const int row = std::clamp(static_cast<int>((1.0f - ny) * (height - 1) + 0.5f), 0, height - 1);
                                return rgba[(static_cast<size_t>(row) * width + col) * 4 + 3] / 255.0f;
                            };
                            auto addHand = [&](std::vector<HandObservation>& hands, const Keypoint& wrist,
                                               const Keypoint& elbow, const Keypoint& shoulder) {
                                if (wrist.conf < confFloor) { return; } // ignore very uncertain wrists
                                const float rawX = wrist.x; // un-mirrored camera space (matches matte/depth)
                                const float rawY = wrist.y;
                                const float alpha = sampleAlpha(rawX, rawY);
                                const float depth = masker->HasDepth() ? masker->SampleDepth(rawX, rawY) : -1.0f;
                                // Matte-alpha weights confidence but never fully kills it (the matte
                                // clips fast-moving hands -- the very thing we track). alphaFloor sets
                                // how much an off-matte wrist keeps.
                                HandObservation obs;
                                obs.x = _videoMirror ? (1.0f - rawX) : rawX;
                                obs.y = rawY;
                                obs.conf = wrist.conf * (alphaFloor + (1.0f - alphaFloor) * alpha);
                                obs.depth = depth;
                                // Raised: wrist above the shoulder (bottom-up y), gated on decent conf.
                                obs.raised = (shoulder.conf > 0.3f)
                                                 ? std::clamp((wrist.y - shoulder.y) * 3.0f + 0.5f, 0.0f, 1.0f)
                                                 : 0.0f;
                                (void)elbow;
                                hands.push_back(obs);
                            };

                            std::vector<HandObservation> hands;
                            hands.reserve(poses.size() * 2);
                            for (const auto& p : poses)
                            {
                                addHand(hands, p[Kpt::LeftWrist], p[Kpt::LeftElbow], p[Kpt::LeftShoulder]);
                                addHand(hands, p[Kpt::RightWrist], p[Kpt::RightElbow], p[Kpt::RightShoulder]);
                            }

                            {
                                std::lock_guard<std::mutex> lock(_poseMutex);
                                _poseHands.swap(hands);
                                _poseFresh = true;
                            }
                        }
                    },
                    preferredDevices,
                    static_cast<double>(_fps), desktopAspect());
                if (segOk)
                {
                    SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                                "ONNX person-seg producing mask in alpha (preset chooses via video_alpha_mode).");
                    return;
                }
                SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION,
                            "Video Mask=seg: webcam failed to start; falling back.");
            }
        }
    }

    // Luxonis OAK depth-camera path: if a preferred device names an OAK/Luxonis
    // unit and depthai support is built in (ENABLE_LUXONIS), use the depth
    // backend. It composites a depth-derived foreground mask into alpha (a mask
    // producer, like seg); the preset decides what to do with it. On failure
    // (no device / not built in) fall through to the plain webcam path.
    bool wantDepth = false;
    for (const auto& d : preferredDevices)
    {
        std::string lo = d;
        std::transform(lo.begin(), lo.end(), lo.begin(),
                       [](unsigned char ch) { return static_cast<char>(std::tolower(ch)); });
        if (lo.find("oak") != std::string::npos || lo.find("luxonis") != std::string::npos)
        {
            wantDepth = true;
            break;
        }
    }
    if (wantDepth)
    {
        if (!DepthCapture::IsSupported())
        {
            SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION,
                        "Luxonis OAK requested but depthai support is not built in "
                        "(configure with -DENABLE_LUXONIS=ON). Falling back to webcam.");
        }
        else
        {
            if (!_depthCapture)
            {
                _depthCapture = std::make_unique<DepthCapture>();
            }
            // Producer only: the depth band fills alpha; the preset's
            // video_alpha_mode (default 0/Source) decides whether to use it.
            const bool depthOk = _depthCapture->Start(
                [handle](const void* data, int width, int height) {
                    projectm_video_submit_frame(handle, data, static_cast<unsigned int>(width),
                                                static_cast<unsigned int>(height),
                                                PROJECTM_VIDEO_FORMAT_RGBA);
                });
            if (depthOk)
            {
                SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                            "Luxonis OAK depth producing mask in alpha (preset chooses via video_alpha_mode).");
                return;
            }
            SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION,
                        "Luxonis OAK capture failed to start; falling back to webcam.");
        }
    }

    const bool ok = _videoCapture->Start(
        [handle](const void* data, int width, int height, VideoCapture::PixelFormat fmt) {
            projectm_video_format pmFmt = PROJECTM_VIDEO_FORMAT_BGRA;
            switch (fmt)
            {
                case VideoCapture::PixelFormat::BGRA: pmFmt = PROJECTM_VIDEO_FORMAT_BGRA; break;
                case VideoCapture::PixelFormat::BGRX: pmFmt = PROJECTM_VIDEO_FORMAT_BGRX; break;
            }
            projectm_video_submit_frame(handle, data, static_cast<unsigned int>(width),
                                        static_cast<unsigned int>(height), pmFmt);
        },
        preferredDevices,
        static_cast<double>(_fps), desktopAspect());

    if (!ok)
    {
        SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION, "Video capture failed to start (permission denied or no device).");
    }
    else
    {
        SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, "Video capture started.");
    }
}

void projectMSDL::stopVideoCapture()
{
    // Stop both backends and (inside Stop) drain in-flight callbacks/threads
    // before any submit_frame / SegMasker state can be torn down.
    if (_videoCapture)
    {
        _videoCapture->Stop();
    }
    if (_depthCapture)
    {
        _depthCapture->Stop();
    }
}

void projectMSDL::toggleVideoCapture()
{
    if (_videoCapture && _videoCapture->IsRunning())
    {
        stopVideoCapture();
        SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, "Video capture stopped.");
    }
    else
    {
        startVideoCapture();
    }
}

#endif

/* Stretch projectM across multiple monitors */
void projectMSDL::stretchMonitors()
{
    int displayCount = SDL_GetNumVideoDisplays();
    if (displayCount >= 2)
    {
        std::vector<SDL_Rect> displayBounds;
        for (int i = 0; i < displayCount; i++)
        {
            displayBounds.push_back(SDL_Rect());
            SDL_GetDisplayBounds(i, &displayBounds.back());
        }

        int mostXLeft = 0;
        int mostXRight = 0;
        int mostYUp = 0;
        int mostYDown = 0;

        for (int i = 0; i < displayCount; i++)
        {
            if (displayBounds[i].x < mostXLeft)
            {
                mostXLeft = displayBounds[i].x;
            }
            if ((displayBounds[i].x + displayBounds[i].w) > mostXRight)
            {
                mostXRight = displayBounds[i].x + displayBounds[i].w;
            }
        }
        for (int i = 0; i < displayCount; i++)
        {
            if (displayBounds[i].y < mostYUp)
            {
                mostYUp = displayBounds[i].y;
            }
            if ((displayBounds[i].y + displayBounds[i].h) > mostYDown)
            {
                mostYDown = displayBounds[i].y + displayBounds[i].h;
            }
        }

        int mostWide = abs(mostXLeft) + abs(mostXRight);
        int mostHigh = abs(mostYUp) + abs(mostYDown);

        SDL_SetWindowPosition(_sdlWindow, mostXLeft, mostYUp);
        SDL_SetWindowSize(_sdlWindow, mostWide, mostHigh);
    }
}

/* Moves projectM to the next monitor */
void projectMSDL::nextMonitor()
{
    int displayCount = SDL_GetNumVideoDisplays();
    int currentWindowIndex = SDL_GetWindowDisplayIndex(_sdlWindow);
    if (displayCount >= 2)
    {
        std::vector<SDL_Rect> displayBounds;
        int nextWindow = currentWindowIndex + 1;
        if (nextWindow >= displayCount)
        {
            nextWindow = 0;
        }

        for (int i = 0; i < displayCount; i++)
        {
            displayBounds.push_back(SDL_Rect());
            SDL_GetDisplayBounds(i, &displayBounds.back());
        }
        SDL_SetWindowPosition(_sdlWindow, displayBounds[nextWindow].x, displayBounds[nextWindow].y);
        SDL_SetWindowSize(_sdlWindow, displayBounds[nextWindow].w, displayBounds[nextWindow].h);
    }
}

void projectMSDL::toggleFullScreen()
{
    if (_isFullScreen)
    {
        SDL_SetWindowFullscreen(_sdlWindow, 0);
        _isFullScreen = false;
        SDL_ShowCursor(true);
    }
    else
    {
        SDL_ShowCursor(false);
        SDL_SetWindowFullscreen(_sdlWindow, SDL_WINDOW_FULLSCREEN_DESKTOP);
        _isFullScreen = true;
    }
}

void projectMSDL::scrollHandler(SDL_Event* sdl_evt)
{
    // handle mouse scroll wheel - up++
    if (sdl_evt->wheel.y > 0)
    {
        projectm_playlist_play_previous(_playlist, true);
    }
    // handle mouse scroll wheel - down--
    if (sdl_evt->wheel.y < 0)
    {
        projectm_playlist_play_next(_playlist, true);
    }
}

void projectMSDL::keyHandler(SDL_Event* sdl_evt)
{
    SDL_Keymod sdl_mod = (SDL_Keymod) sdl_evt->key.keysym.mod;
    SDL_Keycode sdl_keycode = sdl_evt->key.keysym.sym;

    // Left or Right Gui or Left Ctrl
    if (sdl_mod & KMOD_LGUI || sdl_mod & KMOD_RGUI || sdl_mod & KMOD_LCTRL)
    {
        keymod = true;
    }

    // handle keyboard input (for our app first, then projectM)
    switch (sdl_keycode)
    {
        case SDLK_a:
            projectm_set_aspect_correction(_projectM, !projectm_get_aspect_correction(_projectM));
            break;

        case SDLK_F12:
            // Deferred to the next render pass: the frame on screen has already been
            // swapped away, and the capture must read the back buffer before a swap.
            _shotRequested = true;
            break;

        case SDLK_q:
            if (sdl_mod & KMOD_LGUI || sdl_mod & KMOD_RGUI || sdl_mod & KMOD_LCTRL)
            {
                // cmd/ctrl-q = quit
                done = 1;
                return;
            }
            break;

        case SDLK_i:
            if (sdl_mod & KMOD_LGUI || sdl_mod & KMOD_RGUI || sdl_mod & KMOD_LCTRL)
            {
                toggleAudioInput();
                return; // handled
            }
            break;

        case SDLK_s:
            if (sdl_mod & KMOD_LGUI || sdl_mod & KMOD_RGUI || sdl_mod & KMOD_LCTRL)
            {
                // command-s: [s]tretch monitors
                // Stereo requires fullscreen
#if !STEREOSCOPIC_SBS
                if (!this->stretch)
                { // if stretching is not already enabled, enable it.
                    stretchMonitors();
                    this->stretch = true;
                }
                else
                {
                    toggleFullScreen(); // else, just toggle full screen so we leave stretch mode.
                    this->stretch = false;
                }
#endif
                return; // handled
            }

        case SDLK_m:
            if (sdl_mod & KMOD_LGUI || sdl_mod & KMOD_RGUI || sdl_mod & KMOD_LCTRL)
            {
                // command-m: change [m]onitor
                // Stereo requires fullscreen
#if !STEREOSCOPIC_SBS
                nextMonitor();
#endif
                this->stretch = false; // if we are switching monitors, ensure we disable monitor stretching.
                return;                // handled
            }

        case SDLK_f:
            if (sdl_mod & KMOD_LGUI || sdl_mod & KMOD_RGUI || sdl_mod & KMOD_LCTRL)
            {
                // command-f: fullscreen
                // Stereo requires fullscreen
#if !STEREOSCOPIC_SBS
                toggleFullScreen();
#endif
                this->stretch = false; // if we are toggling fullscreen, ensure we disable monitor stretching.
                return;                // handled
            }
            addCurrentPresetToFavorites();
            break;

        case SDLK_r:
            if (sdl_mod & KMOD_LGUI || sdl_mod & KMOD_RGUI || sdl_mod & KMOD_LCTRL)
            {
                // cmd/ctrl-r: reload the current preset from disk (handy while editing a .milk).
                projectm_playlist_set_position(_playlist, projectm_playlist_get_position(_playlist), true);
                return; // handled
            }
            // Use playlist shuffle to randomize.
            projectm_playlist_set_shuffle(_playlist, true);
            projectm_playlist_play_next(_playlist, true);
            projectm_playlist_set_shuffle(_playlist, _shuffle);
            break;

        case SDLK_y:
            _shuffle = !_shuffle;
            projectm_playlist_set_shuffle(_playlist, _shuffle);
            break;

        case SDLK_LEFT:
            projectm_playlist_play_previous(_playlist, true);
            break;

        case SDLK_RIGHT:
            projectm_playlist_play_next(_playlist, true);
            break;

        case SDLK_UP:
            projectm_set_beat_sensitivity(_projectM, projectm_get_beat_sensitivity(_projectM) + 0.01f);
            break;

        case SDLK_DOWN:
            projectm_set_beat_sensitivity(_projectM, projectm_get_beat_sensitivity(_projectM) - 0.01f);
            break;

        case SDLK_SPACE:
            projectm_set_preset_locked(_projectM, !projectm_get_preset_locked(_projectM));
            UpdateWindowTitle();
            break;

#ifdef PROJECTM_VIDEO_CAPTURE_ENABLED
        case SDLK_v:
            toggleVideoCapture();
            break;
#endif

    }
}

void projectMSDL::addFakePCM()
{
    int i;
    int16_t pcm_data[2 * 512];
    /** Produce some fake PCM data to stuff into projectM */
    for (i = 0; i < 512; i++)
    {
        if (i % 2 == 0)
        {
            pcm_data[2 * i] = (float) (rand() / ((float) RAND_MAX) * (pow(2, 14)));
            pcm_data[2 * i + 1] = (float) (rand() / ((float) RAND_MAX) * (pow(2, 14)));
        }
        else
        {
            pcm_data[2 * i] = (float) (rand() / ((float) RAND_MAX) * (pow(2, 14)));
            pcm_data[2 * i + 1] = (float) (rand() / ((float) RAND_MAX) * (pow(2, 14)));
        }
        if (i % 2 == 1)
        {
            pcm_data[2 * i] = -pcm_data[2 * i];
            pcm_data[2 * i + 1] = -pcm_data[2 * i + 1];
        }
    }

    /** Add the waveform data */
    projectm_pcm_add_int16(_projectM, pcm_data, 512, PROJECTM_STEREO);
}

void projectMSDL::resize(unsigned int width_, unsigned int height_)
{
    _width = width_;
    _height = height_;

    // Hide cursor if window size equals desktop size
    SDL_DisplayMode dm;
    if (SDL_GetDesktopDisplayMode(0, &dm) == 0)
    {
        SDL_ShowCursor(_isFullScreen ? SDL_DISABLE : SDL_ENABLE);
    }

    applyRenderSize();
}

void projectMSDL::applyRenderSize()
{
    // Keep the internal render height within [1080, 2160] with a single step from native:
    // double if below, halve if above, otherwise render 1:1. A value still outside the range
    // after one step is left as-is (per design). $PROJECTM_SUPERSAMPLE forces a fixed factor.
    _ssScale = 1.0;
    if (const char* s = std::getenv("PROJECTM_SUPERSAMPLE"); s && s[0])
    {
        const double forced = std::atof(s);
        if (forced > 0.0)
        {
            _ssScale = std::min(4.0, std::max(0.25, forced));
        }
    }
    else if (_height < 1080)
    {
        _ssScale = 2.0;
    }
    else if (_height > 2160)
    {
        _ssScale = 0.5;
    }

    _ssWidth = static_cast<size_t>(std::lround(static_cast<double>(_width) * _ssScale));
    _ssHeight = static_cast<size_t>(std::lround(static_cast<double>(_height) * _ssScale));

    projectm_set_window_size(_projectM, _ssWidth, _ssHeight);
    ensureSupersampleTarget();

    SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                "[render] window %zux%zu -> internal %zux%zu (scale %.2f%s)",
                _width, _height, _ssWidth, _ssHeight, _ssScale,
                usesSupersampleTarget() ? "" : ", direct");
}

void projectMSDL::ensureSupersampleTarget()
{
    if (!usesSupersampleTarget())
    {
        // Rendering 1:1 to FBO 0; tear down any target left over from a prior size.
        if (_ssFbo != 0) { glDeleteFramebuffers(1, &_ssFbo); _ssFbo = 0; }
        if (_ssColorTex != 0) { glDeleteTextures(1, &_ssColorTex); _ssColorTex = 0; }
        if (_ssDepthRbo != 0) { glDeleteRenderbuffers(1, &_ssDepthRbo); _ssDepthRbo = 0; }
        return;
    }

    if (_ssColorTex == 0) { glGenTextures(1, &_ssColorTex); }
    glBindTexture(GL_TEXTURE_2D, _ssColorTex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, static_cast<GLsizei>(_ssWidth), static_cast<GLsizei>(_ssHeight),
                 0, GL_RGBA, GL_UNSIGNED_BYTE, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    if (_ssDepthRbo == 0) { glGenRenderbuffers(1, &_ssDepthRbo); }
    glBindRenderbuffer(GL_RENDERBUFFER, _ssDepthRbo);
    glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT24,
                          static_cast<GLsizei>(_ssWidth), static_cast<GLsizei>(_ssHeight));

    if (_ssFbo == 0) { glGenFramebuffers(1, &_ssFbo); }
    glBindFramebuffer(GL_FRAMEBUFFER, _ssFbo);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, _ssColorTex, 0);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, _ssDepthRbo);

    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
    {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                     "supersample FBO incomplete; falling back to direct 1:1 rendering");
        glBindFramebuffer(GL_FRAMEBUFFER, 0);
        glDeleteFramebuffers(1, &_ssFbo); _ssFbo = 0;
        glDeleteTextures(1, &_ssColorTex); _ssColorTex = 0;
        glDeleteRenderbuffers(1, &_ssDepthRbo); _ssDepthRbo = 0;
        // Render 1:1 instead, and tell projectM to match the window so the direct path is correct.
        _ssScale = 1.0;
        _ssWidth = _width;
        _ssHeight = _height;
        projectm_set_window_size(_projectM, _ssWidth, _ssHeight);
        return;
    }

    glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

void projectMSDL::pollEvent()
{
    SDL_Event evt;

    float mousexscale = 0;
    float mouseyscale = 0;
    int mousepressure = 0;
    while (SDL_PollEvent(&evt))
    {
        switch (evt.type)
        {
            case SDL_WINDOWEVENT:
                int h, w;
                SDL_GL_GetDrawableSize(_sdlWindow, &w, &h);
                switch (evt.window.event)
                {
                    case SDL_WINDOWEVENT_RESIZED:
                        resize(w, h);
                        break;
                    case SDL_WINDOWEVENT_SIZE_CHANGED:
                        resize(w, h);
                        break;
                }
                break;
            case SDL_MOUSEWHEEL:
                scrollHandler(&evt);

            case SDL_KEYDOWN:
                keyHandler(&evt);
                break;

            case SDL_MOUSEBUTTONDOWN:
                if (evt.button.button == SDL_BUTTON_LEFT)
                {
                    // if it's the first mouse down event (since mouse up or since SDL was launched)
                    if (!mouseDown)
                    {
                        // Normalize to [0,1] against the window point size (HiDPI-correct).
                        normalizedMouse(mousexscale, mouseyscale);
                        // Touch. By not supplying a touch type, we will default to random.
                        touch(mousexscale, mouseyscale, mousepressure);
                        mouseDown = true;
                    }
                }
                else if (evt.button.button == SDL_BUTTON_RIGHT)
                {
                    mouseDown = false;

                    // Keymod = Left or Right Gui or Left Ctrl. This is a shortcut to remove all waveforms.
                    if (keymod)
                    {
                        touchDestroyAll();
                        keymod = false;
                        break;
                    }

                    // Right Click — normalize (HiDPI-correct) and destroy at that point.
                    normalizedMouse(mousexscale, mouseyscale);
                    touchDestroy(mousexscale, mouseyscale);
                }
                break;

            case SDL_MOUSEBUTTONUP:
                // On left-button release, end the touch so touch_on falls back to 0 (the point is
                // "up"). This mirrors the pose bridge's UP state and keeps the touch_* built-ins
                // tracking "is the pointer down" rather than latching on after a single click.
                if (evt.button.button == SDL_BUTTON_LEFT && mouseDown)
                {
                    normalizedMouse(mousexscale, mouseyscale);
                    touchDestroy(mousexscale, mouseyscale);
                }
                mouseDown = false;
                break;

            case SDL_QUIT:
                done = true;
                break;
        }
    }

    // Handle dragging your waveform when mouse is down.
    if (mouseDown)
    {
        // Normalize to [0,1] against the window point size (HiDPI-correct), then drag.
        normalizedMouse(mousexscale, mouseyscale);
        touchDrag(mousexscale, mouseyscale, mousepressure);
    }
}

// Normalize the current mouse position to [0,1], Y bottom-to-top. Divides by the window's POINT
// size (SDL_GetWindowSize) rather than _width/_height, which hold the drawable PIXEL size — on
// HiDPI/Retina those differ by the display scale, so using pixels here squashes the range (e.g.
// 0..0.5 at 2x). Mouse coordinates are in points, so points/points gives a correct [0,1].
void projectMSDL::normalizedMouse(float& x, float& y)
{
    int mx = 0, my = 0;
    SDL_GetMouseState(&mx, &my);
    int winW = 0, winH = 0;
    SDL_GetWindowSize(_sdlWindow, &winW, &winH);
    if (winW <= 0 || winH <= 0)
    {
        x = 0.0f;
        y = 0.0f;
        return;
    }
    x = static_cast<float>(mx) / static_cast<float>(winW);
    y = static_cast<float>(winH - my) / static_cast<float>(winH);
}

// This touches the screen to generate a waveform at X / Y.
void projectMSDL::touch(float x, float y, int pressure, int touchtype)
{
#ifdef PROJECTM_TOUCH_ENABLED
    projectm_touch(_projectM, x, y, pressure, static_cast<projectm_touch_type>(touchtype));
#endif
}

// This moves the X Y of your existing waveform that was generated by a touch (only if you held down your click and dragged your mouse around).
void projectMSDL::touchDrag(float x, float y, int pressure)
{
    projectm_touch_drag(_projectM, x, y, pressure);
}

// Remove waveform at X Y
void projectMSDL::touchDestroy(float x, float y)
{
    projectm_touch_destroy(_projectM, x, y);
}

// Remove all waveforms
void projectMSDL::touchDestroyAll()
{
    projectm_touch_destroy_all(_projectM);
}

void projectMSDL::playInitialPreset()
{
    // If the playlist has any presets, jump straight to the first one instead of leaving the
    // built-in idle preset up. set_position ignores shuffle and the preset lock, so this also
    // works when the lock happens to be on at startup. Subsequent advances still respect both.
    if (projectm_playlist_size(_playlist) > 0)
    {
        projectm_playlist_set_position(_playlist, 0, true);
    }
}

void projectMSDL::drainPoseTouch()
{
#ifdef PROJECTM_VIDEO_CAPTURE_ENABLED
    if (!_poseBridge)
    {
        return;
    }

    std::vector<HandObservation> hands;
    {
        std::lock_guard<std::mutex> lock(_poseMutex);
        if (!_poseFresh)
        {
            return; // no new pose frame since last drain; leave touch_* as-is
        }
        hands.swap(_poseHands);
        _poseFresh = false;
    }

    const auto now = std::chrono::steady_clock::now();
    float dt = 1.0f / std::max(1.0f, static_cast<float>(_fps));
    if (_poseDrainInit)
    {
        dt = std::chrono::duration<float>(now - _poseLastDrain).count();
        dt = std::clamp(dt, 1.0e-3f, 0.25f); // guard first-frame / stall spikes
    }
    _poseLastDrain = now;
    _poseDrainInit = true;

    const TouchCommand cmd = _poseBridge->Update(hands, dt);
    const int pressure = static_cast<int>(std::lround(cmd.pressure));
    switch (cmd.op)
    {
        case TouchOp::Down: touch(cmd.x, cmd.y, pressure); break;
        case TouchOp::Drag: touchDrag(cmd.x, cmd.y, pressure); break;
        case TouchOp::Up: touchDestroy(cmd.x, cmd.y); break;
        case TouchOp::None: break;
    }

    // TEMP debug (bridge bring-up): throttled arbitration summary. PROJECTM_POSE_DEBUG_EVERY (0=off).
    const char* everyEnv = std::getenv("PROJECTM_POSE_DEBUG_EVERY");
    const int every = (everyEnv && everyEnv[0]) ? std::atoi(everyEnv) : 0; // 0 = off by default
    static int dbg = 0;
    if (every > 0 && (dbg++ % every) == 0)
    {
        const char* opName = (cmd.op == TouchOp::Down) ? "DOWN"
                             : (cmd.op == TouchOp::Drag) ? "DRAG"
                             : (cmd.op == TouchOp::Up)   ? "UP"
                                                         : "none";
        std::string trackStr;
        char buf[96];
        for (const auto& t : _poseBridge->DebugTracks())
        {
            std::snprintf(buf, sizeof(buf), " %s#%d(x%.2f,spd%.2f,sc%.2f%s)",
                          t.owner ? "*" : "", t.id, t.x, t.speed, t.score, t.active ? ",ON" : "");
            trackStr += buf;
        }
        SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, "[BridgeDebug] %-4s (%.2f,%.2f) |%s",
                    opName, cmd.x, cmd.y, trackStr.c_str());
    }
#endif
}

void projectMSDL::renderFrame()
{
    const auto frameStart = std::chrono::steady_clock::now();

    // Apply the latest pose-driven touch (main thread; projectm_touch* is not thread-safe).
    drainPoseTouch();

    if (usesSupersampleTarget() && _ssFbo != 0)
    {
        // Render the frame at the supersampled resolution into our offscreen FBO...
        glBindFramebuffer(GL_FRAMEBUFFER, _ssFbo);
        glViewport(0, 0, static_cast<GLsizei>(_ssWidth), static_cast<GLsizei>(_ssHeight));
        glClearColor(0.0, 0.0, 0.0, 0.0);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        projectm_opengl_render_frame_fbo(_projectM, _ssFbo);

        // ...then resample it onto the window (linear filter = SSAA box-ish downsample).
        glBindFramebuffer(GL_READ_FRAMEBUFFER, _ssFbo);
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
        glBlitFramebuffer(0, 0, static_cast<GLsizei>(_ssWidth), static_cast<GLsizei>(_ssHeight),
                          0, 0, static_cast<GLsizei>(_width), static_cast<GLsizei>(_height),
                          GL_COLOR_BUFFER_BIT, GL_LINEAR);
        glBindFramebuffer(GL_FRAMEBUFFER, 0);
    }
    else
    {
        glClearColor(0.0, 0.0, 0.0, 0.0);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        projectm_opengl_render_frame(_projectM);
    }

    // Screenshots read the BACK buffer, so they must happen before the swap.
    serviceScreenshots();

    SDL_GL_SwapWindow(_sdlWindow);

    trackFrameRate(frameStart);
}

void projectMSDL::initScreenshots()
{
    if (const char* dir = getenv("PROJECTM_SCREENSHOT_DIR"))
    {
        _shotDir = dir;
    }

    // PROJECTM_SCREENSHOT_AT="5,8,11" -> capture at those many seconds after startup.
    if (const char* at = getenv("PROJECTM_SCREENSHOT_AT"))
    {
        std::stringstream ss(at);
        std::string item;
        while (std::getline(ss, item, ','))
        {
            try
            {
                _shotTimes.push_back(std::stod(item));
            }
            catch (const std::exception&)
            {
                SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION,
                            "[screenshot] Ignoring bad PROJECTM_SCREENSHOT_AT entry: %s", item.c_str());
            }
        }
        std::sort(_shotTimes.begin(), _shotTimes.end());
    }

    // Only meaningful alongside a schedule: quit after the last one is written.
    if (const char* ex = getenv("PROJECTM_SCREENSHOT_EXIT"))
    {
        _shotExit = (std::string(ex) != "0");
    }
}

void projectMSDL::takeScreenshot()
{
    // Name by preset + wall-clock second so successive shots don't overwrite each other.
    std::string preset = getActivePresetName();
    const size_t slash = preset.find_last_of('/');
    if (slash != std::string::npos)
    {
        preset = preset.substr(slash + 1);
    }
    const size_t dot = preset.find_last_of('.');
    if (dot != std::string::npos)
    {
        preset = preset.substr(0, dot);
    }
    // Keep the filename shell-friendly.
    for (auto& c : preset)
    {
        if (!std::isalnum(static_cast<unsigned char>(c)) && c != '-' && c != '_')
        {
            c = '_';
        }
    }

    const auto now = std::chrono::system_clock::now().time_since_epoch();
    const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(now).count();

    const std::string path = _shotDir + "/" + (preset.empty() ? "projectM" : preset) + "-" +
                             std::to_string(ms) + ".png";

    saveScreenshotPng(path, static_cast<int>(_width), static_cast<int>(_height));
}

void projectMSDL::serviceScreenshots()
{
    if (_shotRequested)
    {
        _shotRequested = false;
        takeScreenshot();
    }

    if (_shotIndex >= _shotTimes.size())
    {
        return;
    }

    const auto now = std::chrono::steady_clock::now();
    if (!_startTimeSet)
    {
        _startTime = now;
        _startTimeSet = true;
    }

    const double elapsed = std::chrono::duration<double>(now - _startTime).count();

    // Fire every shot that has come due (and skip any we slept past), so a stalled frame
    // can't silently drop a capture.
    while (_shotIndex < _shotTimes.size() && elapsed >= _shotTimes[_shotIndex])
    {
        takeScreenshot();
        _shotIndex++;
    }

    if (_shotIndex >= _shotTimes.size() && _shotExit)
    {
        SDL_Event quit;
        quit.type = SDL_QUIT;
        SDL_PushEvent(&quit);
    }
}

void projectMSDL::trackFrameRate(std::chrono::steady_clock::time_point frameStart)
{
    const auto now = std::chrono::steady_clock::now();

    if (!_fpsTrackerInitialized)
    {
        _fpsWindowStart = frameStart;
        _fpsTrackerInitialized = true;
    }

    _fpsFrameCount++;
    _fpsFrameMsAccum += std::chrono::duration<double, std::milli>(now - frameStart).count();

    const double windowMs = std::chrono::duration<double, std::milli>(now - _fpsWindowStart).count();
    if (windowMs >= 1000.0)
    {
        const double achievedFps = _fpsFrameCount * 1000.0 / windowMs;
        const double avgFrameMs = _fpsFrameMsAccum / _fpsFrameCount;
        const int target = static_cast<int>(_fps);
        const bool belowTarget = target > 0 && achievedFps < target * 0.9;

        SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                    "[FPS] %5.1f / %d target | %5.2f ms/frame | %s%s",
                    achievedFps, target, avgFrameMs,
                    _presetName.c_str(),
                    belowTarget ? "  <-- BELOW TARGET" : "");

        _fpsWindowStart = now;
        _fpsFrameCount = 0;
        _fpsFrameMsAccum = 0.0;
    }
}

void projectMSDL::init(SDL_Window* window, const bool _renderToTexture)
{
    _sdlWindow = window;
    applyRenderSize();

#ifdef WASAPI_LOOPBACK
    wasapi = true;
#endif

#ifdef PROJECTM_VIDEO_CAPTURE_ENABLED
    startVideoCapture();
#endif
}

std::string projectMSDL::getActivePresetName()
{
    unsigned int index = projectm_playlist_get_position(_playlist);
    if (index)
    {
        auto presetName = projectm_playlist_item(_playlist, index);
        std::string presetNameString(presetName);
        projectm_playlist_free_string(presetName);
        return presetNameString;
    }
    return {};
}

void projectMSDL::addCurrentPresetToFavorites()
{
    const std::string preset = getActivePresetName();
    if (preset.empty())
    {
        return;
    }

    const char* path = "favorites.txt";
    std::ifstream in(path);
    std::string line;
    while (std::getline(in, line))
    {
        if (line == preset)
        {
            SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, "Already in favorites: %s\n", preset.c_str());
            return;
        }
    }
    in.close();

    std::ofstream out(path, std::ios::app);
    if (!out)
    {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "Failed to open %s for append\n", path);
        return;
    }
    out << preset << '\n';
    SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, "Added to favorites: %s\n", preset.c_str());
}

void projectMSDL::presetSwitchedEvent(bool isHardCut, unsigned int index, void* context)
{
    auto app = reinterpret_cast<projectMSDL*>(context);
    auto presetName = projectm_playlist_item(app->_playlist, index);
    SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, "Displaying preset: %s\n", presetName);

    app->_presetName = presetName;
    projectm_playlist_free_string(presetName);

    app->UpdateWindowTitle();
}

projectm_handle projectMSDL::projectM()
{
    return _projectM;
}

void projectMSDL::setFps(size_t fps)
{
    _fps = fps;
}

size_t projectMSDL::fps() const
{
    return _fps;
}

void projectMSDL::setShuffle(bool shuffle)
{
    _shuffle = shuffle;
    projectm_playlist_set_shuffle(_playlist, _shuffle);
}

void projectMSDL::UpdateWindowTitle()
{
    std::string title = "projectM ➫ " + _presetName;
    if (projectm_get_preset_locked(_projectM))
    {
        title.append(" [locked]");
    }
    SDL_SetWindowTitle(_sdlWindow, title.c_str());
}
