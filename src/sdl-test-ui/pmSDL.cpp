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

#include <stb_image.h>

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
    // $PROJECTM_DISPLAY_ASPECT overrides, so the camera mode a different display would
    // negotiate (e.g. 16:9 venue TV vs a 4:3 projector) can be measured from this desk.
    if (const char* env = std::getenv("PROJECTM_DISPLAY_ASPECT"); env && env[0])
    {
        const double a = std::atof(env);
        if (a > 0.0) { return a; }
    }

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

                // Optional NudeNet detector: $PROJECTM_NUDENET_MODEL > "Video Nudity Model". Needs
                // pose (its bbox gates detections to the main figure), so only enable it when pose is
                // active. Throttled in the callback; drives the nude_* eval variables.
                NudeNet* nude = nullptr;
                if (pose && NudeNet::IsSupported())
                {
                    std::string nudeModel;
                    if (const char* env = std::getenv("PROJECTM_NUDENET_MODEL"); env && env[0])
                    {
                        nudeModel = env;
                    }
                    else if (!_nudeModelPath.empty())
                    {
                        nudeModel = _nudeModelPath;
                    }
                    if (!nudeModel.empty())
                    {
                        if (!_nudeNet)
                        {
                            _nudeNet = std::make_unique<NudeNet>();
                        }
                        if (_nudeNet->IsLoaded() || _nudeNet->Load(nudeModel))
                        {
                            nude = _nudeNet.get();
                            SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                                        "NudeNet exposure detection active (nude_* eval variables).");
                        }
                    }
                }

                // Tell presets which optional vision features are live (pose_enabled / nude_enabled /
                // seg_enabled). We are inside the seg-loaded branch, so seg is active; pose and nude
                // are whatever loaded above. Set once here; the library defaults them to 0 otherwise.
                projectm_video_set_capabilities(handle, pose != nullptr, nude != nullptr, /*seg=*/true);

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
                // Forearm extrapolation past the wrist to approximate the hand/fingertip (COCO-17
                // has no finger keypoints). 0 = draw at the wrist itself.
                const float reach = envF("PROJECTM_POSE_REACH", 0.45f);

                // Library owns the mirror (projectm_video_set_mirror), so the matte
                // travels with the RGB either way — seg passes mirror=false.
                const bool segOk = _videoCapture->Start(
                    [handle, masker, outBuf, pose, nude, this, confFloor, alphaFloor, reach](
                        const void* data, int width, int height, VideoCapture::PixelFormat /*fmt*/) {
                        // Per-stage timing for SEG_MASK_PERF.md. Everything in this callback runs
                        // serially on the single capture thread, so these add up to the mask latency.
                        const auto tCbStart = std::chrono::steady_clock::now();

                        masker->Process(static_cast<const uint8_t*>(data), width, height,
                                        /*mirror=*/false, *outBuf);
                        const auto tSegDone = std::chrono::steady_clock::now();

                        // Pose runs AFTER the submit below, so this holds the PREVIOUS frame's
                        // detections -- one frame stale, which is irrelevant either for a debug
                        // marker or for a gate whose threshold is measured in seconds.
                        static std::vector<PersonPose> poses;

                        // --- IDLE STAND-IN ------------------------------------------------
                        // The centroid moved UP here, ahead of the submit, because the idle gate needs
                        // to know whether anyone is in frame before the frame is handed to the library
                        // -- and coverage is what tells it.
                        // Alpha-weighted centroid of the matte (the fallback "center") -> seg_*.
                        // Row 0 is the top of the frame; flip Y so cy matches preset per-pixel
                        // y (bottom-up). The library applies mirror and all smoothing.
                        // The gate is no longer baked into the alpha (it is applied on the GPU), so
                        // weight by it here -- without this the centroid drifts toward the
                        // background people the gate exists to remove.
                        //
                        // Sampled on a stride: a centroid is an integral, so every 4th pixel in each
                        // axis gives the same answer to well under a pixel while costing 1/16th as
                        // much -- which is what makes a per-sample bilinear gate fetch affordable at
                        // all (doing it per pixel is exactly the full-res pass we just removed).
                        const auto tCentroidStart = std::chrono::steady_clock::now();
                        const uint8_t* px = outBuf->data();
                        const bool gated = masker->HasGate();
                        constexpr int kStride = 4;
                        double sumA = 0.0, sumXA = 0.0, sumYA = 0.0;
                        const double invW = (width > 1) ? 1.0 / (width - 1) : 0.0;
                        const double invH = (height > 1) ? 1.0 / (height - 1) : 0.0;
                        int samples = 0;
                        for (int row = 0; row < height; row += kStride)
                        {
                            const double yv = 1.0 - row * invH; // bottom-up
                            for (int col = 0; col < width; col += kStride, ++samples)
                            {
                                double a = px[(static_cast<size_t>(row) * width + col) * 4 + 3] / 255.0;
                                if (gated)
                                {
                                    a *= masker->SampleGate(static_cast<float>(col * invW),
                                                            static_cast<float>(yv));
                                }
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
                            coverage = static_cast<float>(sumA / std::max(1, samples));
                        }
                        const double centroidMs = std::chrono::duration<double, std::milli>(
                                                      std::chrono::steady_clock::now() - tCentroidStart)
                                                      .count();


                        // Nobody in frame? Then (optionally, and only for a deployment that asked for
                        // it) inject a drifting stand-in as the "person", so the visuals have
                        // something to react to. `poses` is last frame's -- one frame stale, which is
                        // nothing against a multi-second gate. Presence is matte OR pose: a performer
                        // who is present but whose matte briefly collapses still counts.
                        const double idleDt = _idleHasTs
                                                  ? std::chrono::duration<double>(tCbStart - _idleLastTs).count()
                                                  : 0.0;
                        _idleLastTs = tCbStart;
                        _idleHasTs = true;
                        const bool personPresent =
                            (coverage > _idleCoverage) ||
                            (!poses.empty() && poses.front().score > 0.4f);
                        applyIdleStandIn(*outBuf, width, height, idleDt, personPresent, cx, cy, coverage);
                        projectm_video_set_seg_idle(handle, _idleActive);

                        // $PROJECTM_SEG_MARKERS=1: stamp WHO THE SYSTEM THINKS THE SUBJECT IS into
                        // the frame. There are three independent answers and they can disagree:
                        //   magenta = the depth gate's ANCHOR (its depth sets the keep band, so this
                        //             is the figure that decides who else gets erased)
                        //   cyan    = pose's primary, which is just poses.front() -- the top NMS
                        //             score, re-picked from scratch every frame
                        // Alpha is forced opaque so the marker survives a mask-only preset (e.g. the
                        // green-screen test) even when it lands off the matte -- which is itself the
                        // tell that the two elections have diverged.

                        static const bool markers = std::getenv("PROJECTM_SEG_MARKERS") != nullptr;
                        if (markers)
                        {
                            auto stamp = [&](float fx, float fy, uint8_t r, uint8_t g, uint8_t b) {
                                const int cx = std::clamp(static_cast<int>(fx * (width - 1)), 0, width - 1);
                                // fy is bottom-up; row 0 is the top of the frame.
                                const int cy = std::clamp(static_cast<int>((1.0f - fy) * (height - 1)),
                                                          0, height - 1);
                                const int half = std::max(4, width / 80);
                                for (int y = std::max(0, cy - half); y <= std::min(height - 1, cy + half); ++y)
                                {
                                    for (int x = std::max(0, cx - half); x <= std::min(width - 1, cx + half); ++x)
                                    {
                                        uint8_t* px = outBuf->data() + (static_cast<size_t>(y) * width + x) * 4;
                                        px[0] = r; px[1] = g; px[2] = b; px[3] = 255;
                                    }
                                }
                            };
                            float ax = 0.0f, ay = 0.0f;
                            if (masker->AnchorCentroid(ax, ay)) { stamp(ax, ay, 255, 0, 255); }
                            if (!poses.empty())
                            {
                                const PersonPose& p = poses.front(); // pose's notion of "primary"
                                stamp(0.5f * (p.boxX0 + p.boxX1), 0.5f * (p.boxY0 + p.boxY1),
                                      0, 255, 255);
                            }
                        }

                        projectm_video_submit_frame(handle, outBuf->data(),
                                                    static_cast<unsigned int>(width),
                                                    static_cast<unsigned int>(height),
                                                    PROJECTM_VIDEO_FORMAT_RGBA);

                        // The depth gate's verdict rides along as a small grid; the library
                        // multiplies it into the matte on the GPU, at texture resolution, instead
                        // of us doing a full-res pass per frame here (SEG_MASK_PERF.md).
                        int gateW = 0, gateH = 0;
                        if (const float* gate = masker->GateGrid(gateW, gateH); gate != nullptr)
                        {
                            projectm_video_submit_alpha_gate(handle, gate,
                                                             static_cast<unsigned int>(gateW),
                                                             static_cast<unsigned int>(gateH));
                        }
                        const auto tSubmitDone = std::chrono::steady_clock::now();

                        // Run body-pose first (if enabled): its torso keypoints give a better
                        // "center" than the matte centroid, and its wrists drive the touch bridge.
                        // (`poses` is declared above the submit, so the debug markers can read the
                        // previous frame's detections.)
                        const auto tPoseStart = std::chrono::steady_clock::now();
                        if (pose)
                        {
                            // Reuse the RGB frame the masker just built from this same BGRA input,
                            // instead of converting the identical frame a second time at full camera
                            // resolution (SEG_MASK_PERF.md, Finding 1 stage 8). Seg runs with
                            // mirror=false, which is the orientation pose wants.
                            const uint8_t* segRgb = masker->RgbFrame();
                            if (segRgb != nullptr)
                            {
                                pose->ProcessRgb(segRgb, width, height, poses);
                            }
                            else
                            {
                                pose->Process(static_cast<const uint8_t*>(data), width, height,
                                              /*mirror=*/false, poses);
                            }
                        }
                        else
                        {
                            poses.clear();
                        }

                        // Make the PRIMARY pose the skeleton that belongs to the body the gate kept.
                        //
                        // Everything downstream takes poses.front() as "the performer", and until now
                        // that was just the top NMS score -- re-elected from scratch every frame, so
                        // two similarly-scored people flipped the whole skeleton (and with it the
                        // touch bridge, and the painting) back and forth between them. Meanwhile the
                        // depth gate had already decided, with depth and salience and hysteresis,
                        // whose body survives in the matte. Those two answers could simply disagree:
                        // the mask kept you and the paint followed someone behind you.
                        //
                        // So score each skeleton by how much of it lands on the anchor component --
                        // confidence-weighted, since a keypoint we barely believe should barely vote
                        // -- and promote the best fit to the front. This is ASSOCIATION, not
                        // election: the gate still chooses the subject on its own evidence. A subject
                        // with no skeleton at all (turned away, crouched, occluded) is unaffected --
                        // we simply have no pose to promote, and fall back to the NMS order.
                        if (poses.size() > 1 && masker->HasGate())
                        {
                            auto fitToAnchor = [&](const PersonPose& p) {
                                float on = 0.0f, total = 0.0f;
                                for (const auto& kp : p.kpts)
                                {
                                    if (kp.conf < 0.2f) { continue; } // too uncertain to vote
                                    total += kp.conf;
                                    if (masker->InAnchor(kp.x, kp.y)) { on += kp.conf; }
                                }
                                return (total > 0.0f) ? (on / total) : 0.0f;
                            };
                            size_t best = 0;
                            float bestFit = fitToAnchor(poses[0]);
                            for (size_t i = 1; i < poses.size(); ++i)
                            {
                                const float fit = fitToAnchor(poses[i]);
                                if (fit > bestFit) { bestFit = fit; best = i; }
                            }
                            // Only override the NMS order on real evidence. If no skeleton overlaps
                            // the anchor (the subject has no pose this frame), leave the order alone
                            // rather than promoting an unrelated person on a tie of zeros.
                            if (best != 0 && bestFit > 0.34f)
                            {
                                std::swap(poses[0], poses[best]);
                                if (markers)
                                {
                                    SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                                                "[Pose] primary <- #%zu of %zu (fit %.2f to the gate's "
                                                "anchor; NMS would have picked #0, fit %.2f)",
                                                best, poses.size(), bestFit, fitToAnchor(poses[best]));
                                }
                            }
                        }

                        const double poseMs = std::chrono::duration<double, std::milli>(
                                                  std::chrono::steady_clock::now() - tPoseStart)
                                                  .count();

                        // seg_cx/seg_cy are EXACTLY the matte centroid -- always, with no pose
                        // dependency. (An earlier version substituted a pose chest point here, which
                        // made seg_cx mean two different things depending on whether a pose model
                        // happened to be loaded. The chest is now pose(JOINT_HEART, ...) instead --
                        // carrying over the shallow-drop tuning that anchor had learned.)
                        projectm_video_set_seg_centroid(handle, cx, cy, coverage);

                        // Submit the skeleton itself: raw COCO-17 plus the derived joints presets
                        // actually want (HEART, hand tips, HEAD, PELVIS). Camera-native X -- the
                        // library applies the mirror. See POSE_API.md.
                        if (pose)
                        {
                            projectm_pose_joint joints[PROJECTM_JOINT_COUNT]{};
                            for (auto& joint : joints)
                            {
                                joint.z = -1.0f; // no depth unless we can sample it
                            }
                            _breastGeomValid = false; // set true only when the shoulder-hip quad is built

                            if (!poses.empty())
                            {
                                const PersonPose& p = poses.front(); // the performer
                                auto put = [&](int idx, float jx, float jy, float jconf) {
                                    joints[idx].x = std::clamp(jx, 0.0f, 1.0f);
                                    joints[idx].y = std::clamp(jy, 0.0f, 1.0f);
                                    joints[idx].z = masker->HasDepth() ? masker->SampleDepth(jx, jy) : -1.0f;
                                    joints[idx].confidence = std::clamp(jconf, 0.0f, 1.0f);
                                };

                                // Raw COCO-17: PersonPose uses the same ordering as the API enum.
                                for (int k = 0; k < kKeypointCount; ++k)
                                {
                                    put(k, p.kpts[k].x, p.kpts[k].y, p.kpts[k].conf);
                                }

                                const Keypoint& ls = p[Kpt::LeftShoulder];
                                const Keypoint& rs = p[Kpt::RightShoulder];
                                const Keypoint& lhip = p[Kpt::LeftHip];
                                const Keypoint& rhip = p[Kpt::RightHip];

                                // PELVIS: hip midpoint.
                                if (lhip.conf > 0.3f && rhip.conf > 0.3f)
                                {
                                    put(PROJECTM_JOINT_PELVIS, (lhip.x + rhip.x) * 0.5f,
                                        (lhip.y + rhip.y) * 0.5f, std::min(lhip.conf, rhip.conf));
                                }

                                // HEART: shoulder midpoint dropped toward the hips (= the quad point at
                                // u=0.5, v=0.28). 0.28 sits mid-upper chest; the older 0.15 read too high.
                                if (ls.conf > 0.4f && rs.conf > 0.4f)
                                {
                                    float hx = (ls.x + rs.x) * 0.5f;
                                    float hy = (ls.y + rs.y) * 0.5f;
                                    if (lhip.conf > 0.3f && rhip.conf > 0.3f)
                                    {
                                        hx += 0.28f * ((lhip.x + rhip.x) * 0.5f - hx);
                                        hy += 0.28f * ((lhip.y + rhip.y) * 0.5f - hy);
                                    }
                                    else
                                    {
                                        hy -= 0.04f; // no hips: nudge just below the shoulder line
                                    }
                                    put(PROJECTM_JOINT_HEART, hx, hy, std::min(ls.conf, rs.conf));
                                }

                                // Hand tips: COCO-17 has no fingers, so extend the forearm past the wrist.
                                auto putHand = [&](int idx, const Keypoint& wrist, const Keypoint& elbow) {
                                    if (wrist.conf <= 0.1f) { return; }
                                    float hx = wrist.x;
                                    float hy = wrist.y;
                                    if (elbow.conf > 0.3f && reach > 0.0f)
                                    {
                                        hx = wrist.x + (wrist.x - elbow.x) * reach;
                                        hy = wrist.y + (wrist.y - elbow.y) * reach;
                                    }
                                    put(idx, hx, hy, wrist.conf);
                                };
                                putHand(PROJECTM_JOINT_L_HAND, p[Kpt::LeftWrist], p[Kpt::LeftElbow]);
                                putHand(PROJECTM_JOINT_R_HAND, p[Kpt::RightWrist], p[Kpt::RightElbow]);

                                // HEAD & CROWN: lift along the head's OWN up-axis (eye-midpoint - nose),
                                // so they follow head tilt and auto-scale with face size -- not a fixed
                                // vertical nudge. Fall back to an ear-based lift if eyes/nose aren't seen.
                                const Keypoint& lear = p[Kpt::LeftEar];
                                const Keypoint& rear = p[Kpt::RightEar];
                                const Keypoint& leye = p[Kpt::LeftEye];
                                const Keypoint& reye = p[Kpt::RightEye];
                                const Keypoint& nose = p[Kpt::Nose];
                                if (leye.conf > 0.3f && reye.conf > 0.3f && nose.conf > 0.3f)
                                {
                                    const float ex = (leye.x + reye.x) * 0.5f, ey = (leye.y + reye.y) * 0.5f;
                                    const float ux = ex - nose.x, uy = ey - nose.y; // up-the-face (rolls with head)
                                    const float hc = std::min(std::min(leye.conf, reye.conf), nose.conf);
                                    put(PROJECTM_JOINT_HEAD,  ex, ey, hc); // head centre
                                    put(PROJECTM_JOINT_CROWN, ex + ux * 2.0f, ey + uy * 2.0f, hc); // top of head
                                }
                                else if (lear.conf > 0.3f && rear.conf > 0.3f)
                                {
                                    const float mx = (lear.x + rear.x) * 0.5f, my = (lear.y + rear.y) * 0.5f;
                                    put(PROJECTM_JOINT_HEAD,  mx, my + 0.06f, std::min(lear.conf, rear.conf));
                                    put(PROJECTM_JOINT_CROWN, mx, my + 0.14f, std::min(lear.conf, rear.conf));
                                }
                                else if (nose.conf > 0.3f)
                                {
                                    put(PROJECTM_JOINT_HEAD, nose.x, nose.y + 0.06f, nose.conf);
                                }

                                // --- Torso points via BILINEAR interpolation of the shoulder-hip quad
                                // (u across 0=left..1=right, v down 0=shoulders..1=hips). Robust to
                                // lean/rotation, unlike a vertical drop. Needs all four corners.
                                if (ls.conf > 0.3f && rs.conf > 0.3f && lhip.conf > 0.3f && rhip.conf > 0.3f)
                                {
                                    const float qc = std::min(std::min(ls.conf, rs.conf),
                                                              std::min(lhip.conf, rhip.conf));
                                    auto quadPt = [&](float u, float v, float& ox, float& oy) {
                                        const float tx = ls.x * (1.0f - u) + rs.x * u;
                                        const float ty = ls.y * (1.0f - u) + rs.y * u;
                                        const float bx = lhip.x * (1.0f - u) + rhip.x * u;
                                        const float by = lhip.y * (1.0f - u) + rhip.y * u;
                                        ox = tx * (1.0f - v) + bx * v;
                                        oy = ty * (1.0f - v) + by * v;
                                    };
                                    auto quad = [&](int idx, float u, float v) {
                                        float px, py;
                                        quadPt(u, v, px, py);
                                        put(idx, px, py, qc);
                                    };
                                    // BREAST: geometric quad estimate + NudeNet-refined offset (twist
                                    // correction, updated at the detector's throttled rate below).
                                    quadPt(0.10f, 0.30f, _breastGeomLX, _breastGeomLY);
                                    quadPt(0.90f, 0.30f, _breastGeomRX, _breastGeomRY);
                                    _breastGeomValid = true;
                                    put(PROJECTM_JOINT_L_BREAST, _breastGeomLX + _breastOffLX,
                                        _breastGeomLY + _breastOffLY, qc);
                                    put(PROJECTM_JOINT_R_BREAST, _breastGeomRX + _breastOffRX,
                                        _breastGeomRY + _breastOffRY, qc);
                                    quad(PROJECTM_JOINT_NAVEL,    0.50f, 0.75f);

                                    // GROIN: hip midpoint pushed below the quad along shoulders->hips
                                    // (geometric estimate) + NudeNet-refined offset (see the nudenet
                                    // step below), exactly like the breasts.
                                    const float smx = (ls.x + rs.x) * 0.5f, smy = (ls.y + rs.y) * 0.5f;
                                    const float hmx = (lhip.x + rhip.x) * 0.5f, hmy = (lhip.y + rhip.y) * 0.5f;
                                    _groinGeomX = hmx + 0.12f * (hmx - smx);
                                    _groinGeomY = hmy + 0.12f * (hmy - smy);
                                    put(PROJECTM_JOINT_GROIN, _groinGeomX + _groinOffX,
                                        _groinGeomY + _groinOffY, qc);
                                }

                                // THROAT: shoulder midpoint raised toward the head (nose = up ref).
                                if (ls.conf > 0.3f && rs.conf > 0.3f && nose.conf > 0.3f)
                                {
                                    const float smx = (ls.x + rs.x) * 0.5f, smy = (ls.y + rs.y) * 0.5f;
                                    put(PROJECTM_JOINT_THROAT, smx + 0.30f * (nose.x - smx),
                                        smy + 0.30f * (nose.y - smy),
                                        std::min(std::min(ls.conf, rs.conf), nose.conf));
                                }

                                // (CROWN is computed with HEAD above, along the face up-axis.)

                                // FINGERTIPS: like the hand tip, reaching FURTHER past the wrist.
                                auto putTip = [&](int idx, const Keypoint& a, const Keypoint& b, float k) {
                                    if (a.conf <= 0.1f) { return; }
                                    float x = a.x, y = a.y;
                                    if (b.conf > 0.3f) { x = a.x + (a.x - b.x) * k; y = a.y + (a.y - b.y) * k; }
                                    put(idx, x, y, a.conf);
                                };
                                putTip(PROJECTM_JOINT_L_FINGER, p[Kpt::LeftWrist],  p[Kpt::LeftElbow],  0.75f);
                                putTip(PROJECTM_JOINT_R_FINGER, p[Kpt::RightWrist], p[Kpt::RightElbow], 0.75f);
                                // FOOT TIPS: extend past the ankle along the shin (knee->ankle).
                                putTip(PROJECTM_JOINT_L_FOOT, p[Kpt::LeftAnkle],  p[Kpt::LeftKnee],  0.30f);
                                putTip(PROJECTM_JOINT_R_FOOT, p[Kpt::RightAnkle], p[Kpt::RightKnee], 0.30f);
                            }

                            projectm_pose_set(handle, joints, PROJECTM_JOINT_COUNT);
                        }

                        // NudeNet (throttled): main-figure exposure -> nude_* eval variables. Runs
                        // every Nth capture frame (PROJECTM_NUDENET_EVERY, ~2-3 Hz) since it is a
                        // full extra inference on the serial capture thread; its hysteresis integrates
                        // over these calls and the library holds the last verdict in between. Gated to
                        // the primary person's bbox; no person -> personValid=false decays it covered.
                        if (nude)
                        {
                            static const int nudeEvery = []() {
                                const char* v = std::getenv("PROJECTM_NUDENET_EVERY");
                                const int n = (v && v[0]) ? std::atoi(v) : 12;
                                return n > 0 ? n : 12;
                            }();
                            static int nudeFrame = 0;
                            if ((nudeFrame++ % nudeEvery) == 0)
                            {
                                const uint8_t* segRgb = masker->RgbFrame();
                                const bool personValid = !poses.empty() && segRgb != nullptr;
                                float bx0 = 0.0f, by0 = 0.0f, bx1 = 0.0f, by1 = 0.0f;
                                if (!poses.empty())
                                {
                                    const PersonPose& p = poses.front();
                                    bx0 = p.boxX0; by0 = p.boxY0; bx1 = p.boxX1; by1 = p.boxY1;
                                }
                                nude->ProcessRgb(segRgb, width, height, bx0, by0, bx1, by1, personValid);
                                projectm_video_set_nudity(handle, nude->Top(), nude->Rear(),
                                                          nude->FrontF(), nude->FrontM(), nude->Female());

                                // Breast-location refinement: EMA a per-side (detected - geometric)
                                // offset toward this run's NudeNet breast boxes, assigning each to the
                                // nearer geometric side. Sides with no detection decay toward 0 (fall
                                // back to geometry). Applied every frame in the quad step above.
                                if (_breastGeomValid)
                                {
                                    const float maxOff = 0.15f; // clamp so a stray box can't fling it
                                    const float a = 0.6f;       // per-run EMA toward the correction
                                    // Confidence-weighted fusion: weak (covered) boxes wander, strong
                                    // (exposed) boxes are precise. w ramps 0..1 across [loW, hiW], so a
                                    // low-confidence box keeps the joint on geometry and a high one
                                    // pulls it fully onto the box. Env-tunable.
                                    static const float loW = []() {
                                        const char* v = std::getenv("PROJECTM_NUDENET_BREAST_W_LO");
                                        return (v && v[0]) ? static_cast<float>(std::atof(v)) : 0.35f;
                                    }();
                                    static const float hiW = []() {
                                        const char* v = std::getenv("PROJECTM_NUDENET_BREAST_W_HI");
                                        return (v && v[0]) ? static_cast<float>(std::atof(v)) : 0.60f;
                                    }();
                                    const float invSpan = 1.0f / std::max(1e-3f, hiW - loW);
                                    bool gotL = false, gotR = false;
                                    const int n = nude->BreastCount();
                                    for (int i = 0; i < n; ++i)
                                    {
                                        float bx = 0.0f, by = 0.0f, bs = 0.0f;
                                        if (!nude->Breast(i, bx, by, bs)) { continue; }
                                        const float w = std::clamp((bs - loW) * invSpan, 0.0f, 1.0f);
                                        const float dL = std::hypot(bx - _breastGeomLX, by - _breastGeomLY);
                                        const float dR = std::hypot(bx - _breastGeomRX, by - _breastGeomRY);
                                        const bool toL = (dL <= dR) ? !gotL : gotR; // prefer nearer, one per side
                                        if (toL)
                                        {
                                            const float ox = std::clamp(bx - _breastGeomLX, -maxOff, maxOff) * w;
                                            const float oy = std::clamp(by - _breastGeomLY, -maxOff, maxOff) * w;
                                            _breastOffLX += (ox - _breastOffLX) * a;
                                            _breastOffLY += (oy - _breastOffLY) * a;
                                            gotL = true;
                                        }
                                        else
                                        {
                                            const float ox = std::clamp(bx - _breastGeomRX, -maxOff, maxOff) * w;
                                            const float oy = std::clamp(by - _breastGeomRY, -maxOff, maxOff) * w;
                                            _breastOffRX += (ox - _breastOffRX) * a;
                                            _breastOffRY += (oy - _breastOffRY) * a;
                                            gotR = true;
                                        }
                                    }
                                    const float decay = 0.5f; // ease back to geometry when unseen
                                    if (!gotL) { _breastOffLX *= decay; _breastOffLY *= decay; }
                                    if (!gotR) { _breastOffRX *= decay; _breastOffRY *= decay; }

                                    // GROIN: same confidence-weighted offset toward the exposed-
                                    // genitalia box; decay to geometry when not detected.
                                    float gx = 0.0f, gy = 0.0f, gsc = 0.0f;
                                    if (nude->GroinBox(gx, gy, gsc))
                                    {
                                        const float w = std::clamp((gsc - loW) * invSpan, 0.0f, 1.0f);
                                        const float ox = std::clamp(gx - _groinGeomX, -maxOff, maxOff) * w;
                                        const float oy = std::clamp(gy - _groinGeomY, -maxOff, maxOff) * w;
                                        _groinOffX += (ox - _groinOffX) * a;
                                        _groinOffY += (oy - _groinOffY) * a;
                                    }
                                    else
                                    {
                                        _groinOffX *= decay;
                                        _groinOffY *= decay;
                                    }
                                }
                            }
                        }

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
                                const float a = rgba[(static_cast<size_t>(row) * width + col) * 4 + 3] / 255.0f;
                                // The gate is applied on the GPU, not baked into this alpha -- weight
                                // it here, or a gated-out spectator's wrist keeps full confidence and
                                // can still drive the touch bridge.
                                return a * masker->SampleGate(nx, ny);
                            };
                            // COCO-17 has no finger keypoints (stops at the wrist), so estimate the
                            // hand/fingertip by extending the forearm past the wrist:
                            //   tip = wrist + (wrist - elbow) * reach.  reach ~0.4 ~= hand/forearm.
                            auto addHand = [&](std::vector<HandObservation>& hands, const Keypoint& wrist,
                                               const Keypoint& elbow, const Keypoint& shoulder) {
                                if (wrist.conf < confFloor) { return; } // ignore very uncertain wrists
                                float rawX = wrist.x; // un-mirrored camera space (matches matte/depth)
                                float rawY = wrist.y;
                                if (elbow.conf > 0.3f && reach > 0.0f)
                                {
                                    rawX = std::clamp(wrist.x + (wrist.x - elbow.x) * reach, 0.0f, 1.0f);
                                    rawY = std::clamp(wrist.y + (wrist.y - elbow.y) * reach, 0.0f, 1.0f);
                                }
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

                        // Per-stage capture-thread cost (SEG_MASK_PERF.md step 1: "instrument first
                        // -- everything below should be confirmed by these numbers"). Everything in
                        // this callback is serial on one thread, so the total IS the mask latency
                        // budget. PROJECTM_SEG_PERF_EVERY=N logs every Nth frame; 0 (default) = off.
                        static const int perfEvery = []() {
                            const char* v = std::getenv("PROJECTM_SEG_PERF_EVERY");
                            return (v && v[0]) ? std::atoi(v) : 0;
                        }();
                        static int perfFrame = 0;
                        if (perfEvery > 0 && (perfFrame++ % perfEvery) == 0)
                        {
                            const auto now = std::chrono::steady_clock::now();
                            auto ms = [](auto from, auto to) {
                                return std::chrono::duration<double, std::milli>(to - from).count();
                            };
                            const SegTimings& st = masker->LastTimings();
                            SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                                        "[SegPerf] %dx%d | TOTAL %.1f ms || seg %.1f (rgb %.1f, infer %.1f, "
                                        "composite %.1f, harden %.1f, depth %.1f) | submit %.1f | pose %.1f | "
                                        "centroid %.1f | rest %.1f",
                                        width, height, ms(tCbStart, now),
                                        st.totalMs, st.rgbMs, st.inferMs, st.compositeMs, st.hardenMs,
                                        st.depthMs, ms(tSegDone, tSubmitDone), poseMs, centroidMs,
                                        ms(tCbStart, now) - st.totalMs - ms(tSegDone, tSubmitDone) -
                                            poseMs - centroidMs);
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
            // Shift+F12 grabs the OTHER surface, so the two are easy to compare.
            if (sdl_mod & KMOD_SHIFT)
            {
                std::swap(_shotComposite, _shotMain);
                if (!_shotComposite && !_shotMain)
                {
                    _shotComposite = true;
                }
            }
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
            // f (with or without cmd/ctrl): fullscreen, as in every other app.
            // Favouriting used to live on the UNMODIFIED f, so reaching for fullscreen and missing
            // the modifier silently wrote a favorite -- which quietly filled favorites.txt with
            // presets nobody chose. It is now on cmd/ctrl-d ("bookmark"), below.
            // Stereo requires fullscreen
#if !STEREOSCOPIC_SBS
            toggleFullScreen();
#endif
            this->stretch = false; // if we are toggling fullscreen, ensure we disable monitor stretching.
            return;                // handled

        case SDLK_d:
            if (sdl_mod & KMOD_LGUI || sdl_mod & KMOD_RGUI || sdl_mod & KMOD_LCTRL)
            {
                // cmd/ctrl-d: add the current preset to favorites. Deliberately requires a
                // modifier: this writes to a file, so it should not be a bare keypress.
                addCurrentPresetToFavorites();
                return; // handled
            }
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

        // Left/Right switch preset. Plain = a HARD cut (instant), Shift = a SOFT cut, i.e. it plays
        // the transition. Without this there is no way to see a transition on demand: every manual
        // switch was a hard cut, so transitions only ever appeared when a preset timed out by itself
        // -- which makes writing or reviewing one needlessly painful.
        case SDLK_LEFT:
            projectm_playlist_play_previous(_playlist, !(sdl_mod & KMOD_SHIFT));
            break;

        case SDLK_RIGHT:
            projectm_playlist_play_next(_playlist, !(sdl_mod & KMOD_SHIFT));
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

    // PROJECTM_SCREENSHOT_SURFACE = comp (default) | main | both
    //   comp: the window -- what the viewer actually sees, composite shader and all.
    //   main: the preset's PRE-composite drawing (what the warp stage produced; also next frame's
    //         sampler_main). Use this when reasoning about the warp shader: a composite that crops,
    //         curves or shades the image will otherwise confuse what you are trying to learn.
    if (const char* surface = getenv("PROJECTM_SCREENSHOT_SURFACE"))
    {
        const std::string value(surface);
        _shotComposite = (value == "comp" || value == "both");
        _shotMain = (value == "main" || value == "both");
        if (!_shotComposite && !_shotMain)
        {
            SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION,
                        "[screenshot] Unknown PROJECTM_SCREENSHOT_SURFACE '%s' (want comp|main|both); using comp",
                        surface);
            _shotComposite = true;
        }
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

    const std::string stem = _shotDir + "/" + (preset.empty() ? "projectM" : preset) + "-" +
                             std::to_string(ms);

    // Suffix only when both are written, so the common (composite-only) case keeps clean names.
    const bool both = _shotComposite && _shotMain;

    if (_shotComposite)
    {
        saveScreenshotPng(stem + (both ? "-comp.png" : ".png"),
                          static_cast<int>(_width), static_cast<int>(_height));
    }
    if (_shotMain)
    {
        saveTexturePng(stem + (both ? "-main.png" : ".png"),
                       projectm_opengl_get_main_texture(_projectM));
    }
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

void projectMSDL::setIdleStandIn(const std::string& imagePath, float delaySeconds, float coverageFloor,
                                 float heightFraction)
{
    _idleImage.clear();
    _idleImageW = _idleImageH = 0;
    _idleDelay = std::max(0.0f, delaySeconds);
    _idleCoverage = std::clamp(coverageFloor, 0.0f, 1.0f);
    _idleScale = std::clamp(heightFraction, 0.02f, 1.0f);
    if (imagePath.empty())
    {
        return; // feature off -- the default for every deployment that did not ask for it
    }

    int w = 0, h = 0, ch = 0;
    stbi_uc* rgba = stbi_load(imagePath.c_str(), &w, &h, &ch, 4);
    if (rgba == nullptr || w <= 0 || h <= 0)
    {
        SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION,
                    "[Idle] Cannot load stand-in image '%s'; idle injection disabled.",
                    imagePath.c_str());
        if (rgba != nullptr) { stbi_image_free(rgba); }
        return;
    }
    _idleImage.assign(rgba, rgba + static_cast<size_t>(w) * h * 4);
    stbi_image_free(rgba);
    _idleImageW = w;
    _idleImageH = h;
    SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                "[Idle] Stand-in '%s' (%dx%d): appears after %.1fs with nobody in frame "
                "(coverage < %.3f), at %.0f%% of frame height.",
                imagePath.c_str(), w, h, _idleDelay, _idleCoverage, _idleScale * 100.0f);
}

bool projectMSDL::applyIdleStandIn(std::vector<uint8_t>& rgba, int w, int h, double dt,
                                   bool personPresent, float& cx, float& cy, float& coverage)
{
    if (_idleImage.empty() || w <= 0 || h <= 0)
    {
        return false;
    }

    // QUICK OFF. The instant anyone is present the stand-in is gone -- no fade, no debounce. A
    // performer stepping in must never have to wait for a logo to get out of their way.
    if (personPresent)
    {
        if (_idleActive)
        {
            SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, "[Idle] OFF -- someone is in frame.");
        }
        _idleAbsentFor = 0.0;
        _idleActive = false;
        return false;
    }

    // RELUCTANT ON. The absence has to persist. Someone standing still, or briefly out of frame,
    // must not summon it. (Once it IS on, it stays on until someone appears -- the delay is the cost
    // of entry, not a per-frame test.)
    _idleAbsentFor += dt;
    if (!_idleActive && _idleAbsentFor < _idleDelay)
    {
        return false;
    }
    if (!_idleActive)
    {
        SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION,
                    "[Idle] ON -- nobody in frame for %.1fs.", _idleAbsentFor);
    }
    _idleActive = true;
    _idleClock += dt;

    // Lissajous drift: two incommensurate frequencies, so the path never repeats and never settles
    // into an obvious loop. Slow -- this is ambience, not motion.
    const double t = _idleClock;
    const float fx = 0.5f + 0.20f * static_cast<float>(std::sin(0.13 * t));
    const float fy = 0.5f + 0.13f * static_cast<float>(std::sin(0.19 * t + 1.7));

    // Height is a fraction of the frame ("Idle Scale"), aspect preserved. Keep it modest: this is a
    // stand-in drifting through an empty room, not a title card -- and every pixel of it is MATTE, so
    // a big one hands the presets a big "person" to warp.
    const float scale = (_idleScale * h) / static_cast<float>(_idleImageH);
    const int dw = std::max(1, static_cast<int>(_idleImageW * scale));
    const int dh = std::max(1, static_cast<int>(_idleImageH * scale));
    const int x0 = static_cast<int>(fx * w) - dw / 2;
    const int y0 = static_cast<int>((1.0f - fy) * h) - dh / 2; // fy is bottom-up; row 0 is the top

    // The matte is the stand-in and NOTHING else: clear the alpha first, or the real (empty-ish)
    // matte's noise would hang around it. RGB is left as the camera's, and the stand-in is composited
    // over it by its own alpha -- so it reads as an object in the room, not a sticker on the lens.
    for (size_t i = 0; i < static_cast<size_t>(w) * h; ++i)
    {
        rgba[i * 4 + 3] = 0;
    }

    double sumA = 0.0, sumX = 0.0, sumY = 0.0;
    for (int y = std::max(0, y0); y < std::min(h, y0 + dh); ++y)
    {
        const int sy = std::clamp(static_cast<int>((y - y0) / scale), 0, _idleImageH - 1);
        for (int x = std::max(0, x0); x < std::min(w, x0 + dw); ++x)
        {
            const int sx = std::clamp(static_cast<int>((x - x0) / scale), 0, _idleImageW - 1);
            const uint8_t* src = &_idleImage[(static_cast<size_t>(sy) * _idleImageW + sx) * 4];
            const float a = src[3] / 255.0f;
            if (a <= 0.0f) { continue; }

            uint8_t* dst = &rgba[(static_cast<size_t>(y) * w + x) * 4];
            for (int c = 0; c < 3; ++c)
            {
                dst[c] = static_cast<uint8_t>(dst[c] * (1.0f - a) + src[c] * a + 0.5f);
            }
            dst[3] = src[3];

            sumA += a;
            sumX += a * (static_cast<double>(x) / std::max(1, w - 1));
            sumY += a * (1.0 - static_cast<double>(y) / std::max(1, h - 1)); // bottom-up
        }
    }

    // Describe the stand-in exactly as if it were a person, so seg_cx/seg_cy/seg_coverage -- and
    // everything that follows the performer -- follow it. `seg_idle` is what tells a preset it is not
    // real; these three must not lie, or a preset that opts IN gets a centroid stuck at the origin.
    if (sumA > 1e-6)
    {
        cx = static_cast<float>(sumX / sumA);
        cy = static_cast<float>(sumY / sumA);
        coverage = static_cast<float>(sumA / (static_cast<double>(w) * h));
    }
    return true;
}
