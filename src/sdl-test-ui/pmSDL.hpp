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
* pmSDL.hpp 
* Authors: Created by Mischa Spiegelmock on 2017-09-18.
*
*/

#pragma once

#include "opengl.h"
#include <SDL2/SDL.h>

// Disable LOOPBACK and FAKE audio to enable microphone input
#ifdef _WIN32
#define WASAPI_LOOPBACK 1
#endif /** _WIN32 */
#define FAKE_AUDIO 0
// ----------------------------
#define TEST_ALL_PRESETS 0
#define STEREOSCOPIC_SBS 0

// projectM
#include <projectM-4/playlist.h>
#include <projectM-4/projectM.h>

// projectM SDL
#include "audioCapture.hpp"
#include "loopback.hpp"
#include "setup.hpp"
#ifdef PROJECTM_VIDEO_CAPTURE_ENABLED
#include "videoCapture.hpp"
#include "depthCapture.hpp"
#include "segMask.hpp"
#include "poseTracker.hpp"
#include "poseTouchBridge.hpp"
#endif

#include <chrono>
#include <memory>
#include <mutex>


#if defined _MSC_VER
#include <direct.h>
#endif

#include <fstream>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <iostream>
#include <string>
#include <sys/stat.h>
#include <vector>

#ifdef WASAPI_LOOPBACK
#include <windows.h>
#include <mmdeviceapi.h>
#include <audioclient.h>

#include <functiondiscoverykeys_devpkey.h>
#include <avrt.h>

#include <mmsystem.h>
#include <stdio.h>


#define LOG(format, ...) wprintf(format L"\n", ##__VA_ARGS__)
#define ERR(format, ...) LOG(L"Error: " format, ##__VA_ARGS__)

#endif /** WASAPI_LOOPBACK */

#ifdef _WIN32
#define SDL_MAIN_HANDLED
#include "SDL.h"
#else
#include <SDL2/SDL.h>
#endif /** _WIN32 */


// DATADIR_PATH should be set by the root Makefile if this is being
// built with autotools.
#ifndef DATADIR_PATH
#ifdef DEBUG
#define DATADIR_PATH "."
#ifndef _WIN32
#warning "DATADIR_PATH is not defined - falling back to ./"
#else
#pragma warning "DATADIR_PATH is not defined - falling back to ./"
#endif /** _WIN32 */
#else
#define DATADIR_PATH "/usr/local/share/projectM"
#ifndef _WIN32
#warning "DATADIR_PATH is not defined - falling back to /usr/local/share/projectM"
#endif /** _WIN32 */
#endif
#endif

class projectMSDL
{

public:
    projectMSDL(SDL_GLContext glCtx, const std::string& presetPath);
    projectMSDL(SDL_GLContext glCtx, const std::vector<std::string>& presetList);

    ~projectMSDL();

    void init(SDL_Window* window, const bool renderToTexture = false);
    int openAudioInput(const char* deviceName = nullptr);
    // Try each preferred device name (case-insensitive substring) in order; the first
    // capture device that matches and opens wins. Falls back to the system default if
    // none match. An empty list behaves like the system-default open.
    int openAudioInput(const std::vector<std::string>& preferredNames);
    int toggleAudioInput();
    int initAudioInput();
    void beginAudioCapture();
    void endAudioCapture();
#ifdef PROJECTM_VIDEO_CAPTURE_ENABLED
    void toggleVideoCapture();
    void startVideoCapture();
    void stopVideoCapture();
#endif
    void stretchMonitors();
    void nextMonitor();
    void toggleFullScreen();
    void resize(unsigned int width, unsigned int height);
    //! Reads the mouse position and normalizes it to [0,1] with Y bottom-to-top (matching the
    //! touch_*/seg_* convention). Uses the window's POINT size, not the drawable pixel size, so
    //! it stays correct on HiDPI/Retina where the two differ by the display scale.
    void normalizedMouse(float& x, float& y);
    void touch(float x, float y, int pressure, int touchtype = 0);
    void touchDrag(float x, float y, int pressure);
    void touchDestroy(float x, float y);
    void touchDestroyAll();
    void renderFrame();
    void playInitialPreset(); //!< Loads the first playlist preset at startup so the idle preset isn't shown when presets are available.
    void trackFrameRate(std::chrono::steady_clock::time_point frameStart); //!< Accumulates and logs the achieved frame rate once per second.
    void pollEvent();
    bool keymod = false;
    std::string getActivePresetName();
    void addCurrentPresetToFavorites();
    void addFakePCM();
    projectm_handle projectM();
    void setFps(size_t fps);
    size_t fps() const;

    // Sets whether the playlist plays in shuffled (random) or sequential order.
    // Set from config ("Shuffle") in setupSDLApp(); the 'y' key still toggles it at runtime.
    void setShuffle(bool shuffle);

    // Preference-ordered video source list (substrings, case-insensitive), set from config
    // before init() because capture starts there. Highest priority first. Audio is opened
    // directly from setupSDLApp(), so its preference list is passed to openAudioInput().
    void setVideoDevicePrefs(const std::vector<std::string>& prefs) { _videoDevicePrefs = prefs; }

    // Foreground-masking preference from config ("Video Mask"): off|source|const|motion|decay|
    // chroma|bgsub|seg, optional "-raw" suffix to skip refinement. $PROJECTM_VIDEO_MASK overrides it.
    void setVideoMaskPref(const std::string& pref) { _videoMaskPref = pref; }

    // ONNX person-seg model path ("Video Seg Model" config); empty = default
    // ($HOME/.projectM/models/rvm_mobilenetv3.onnx). $PROJECTM_SEG_MODEL overrides it.
    void setVideoSegModel(const std::string& path) { _segModelPath = path; }

    // Optional second seg model ("Video Seg Model 2"); its matte multiplies the primary's
    // (e.g. RVM x person mask). Empty = none. $PROJECTM_SEG_MODEL2 overrides it.
    void setVideoSegModel2(const std::string& path) { _segModelPath2 = path; }

    // How the 2nd model combines ("Video Seg Combine"): "multiply" (default) or "gate".
    // $PROJECTM_SEG_COMBINE overrides.
    void setVideoSegCombine(const std::string& mode) { _segCombine = mode; }

    // ONNX person-seg quality level ("Video Seg Quality" config): 1=fast/256,
    // 2=balanced/384, 3=quality/512. 0 = unset (defaults to 2). Higher = crisper
    // matte (thin limbs steadier) but slower. $PROJECTM_SEG_QUALITY overrides.
    void setVideoSegQuality(int quality) { _segQuality = quality; }

    // Optional monocular depth model ("Video Seg Depth Model"): when set, background people
    // (spectators/passers-by) are dropped from the matte by relative depth. Empty = off.
    // $PROJECTM_SEG_DEPTH_MODEL overrides it.
    void setVideoSegDepthModel(const std::string& path) { _segDepthModel = path; }

    // Depth keep band ("Video Seg Depth Band", 0..1): how far behind the nearest person still
    // counts as "front". Larger keeps more people. <=0 = unset (default 0.20). $PROJECTM_SEG_DEPTH_BAND overrides.
    void setVideoSegDepthBand(double band) { _segDepthBand = band; }

    // Matte-hardening smoothstep edges ("Video Seg Harden Lo/Hi", 0..1): remap matte alpha so
    // alpha<=lo->0, alpha>=hi->1 (reduces soft-matte ghosting). lo<=0 && hi>=1 = off (raw matte).
    // $PROJECTM_SEG_HARDEN_LO / _HI override these.
    void setVideoSegHardenLo(double lo) { _segHardenLo = lo; }
    void setVideoSegHardenHi(double hi) { _segHardenHi = hi; }

    // ONNX body-pose model path ("Video Pose Model"): when set (with Video Mask=seg), a YOLO-pose
    // model runs alongside seg and drives the pose->touch bridge. Empty = pose off.
    // $PROJECTM_POSE_MODEL overrides it.
    void setVideoPoseModel(const std::string& path) { _poseModelPath = path; }

    // Whether the camera feed is horizontally mirrored ("Video Mirror"). The pose->touch bridge
    // applies the same flip so touch lands where the performer sees their hand.
    void setVideoMirror(bool mirror) { _videoMirror = mirror; }

    bool done{false};
    bool mouseDown{false};
    bool wasapi{false};    // Used to track if wasapi is currently active. This bool will allow us to run a WASAPI app and still toggle to microphone inputs.
    bool fakeAudio{false}; // Used to track fake audio, so we can turn it off and on.
    bool stretch{false};   // used for toggling stretch mode

    SDL_GLContext _openGlContext{nullptr};

private:
    static void presetSwitchedEvent(bool isHardCut, uint32_t index, void* context);

    static void audioInputCallbackF32(void* userdata, unsigned char* stream, int len);

    void UpdateWindowTitle();

    void scrollHandler(SDL_Event*);
    void keyHandler(SDL_Event*);

    projectm_handle _projectM{nullptr};
    projectm_playlist_handle _playlist{nullptr};

    SDL_Window* _sdlWindow{nullptr};
    bool _isFullScreen{false};
    size_t _width{0};
    size_t _height{0};
    size_t _fps{60};

    // Supersampling. projectM renders into an offscreen FBO at _ssWidth x _ssHeight, then we
    // blit (linear) onto the window. The render height is kept within [1080, 2160] by a single
    // x2 / /2 step from native (so _ssScale is one of 0.5, 1, 2); if native is already in range
    // we render 1:1 straight to FBO 0 and skip the offscreen path. $PROJECTM_SUPERSAMPLE (a
    // float scale) forces a fixed factor for tuning. _width/_height stay = window size.
    double _ssScale{1.0};
    size_t _ssWidth{0};
    size_t _ssHeight{0};
    GLuint _ssFbo{0};
    GLuint _ssColorTex{0};
    GLuint _ssDepthRbo{0};
    // True when the internal render size differs from the window, i.e. we need the offscreen path.
    bool usesSupersampleTarget() const { return _ssWidth != _width || _ssHeight != _height; }
    // Picks _ssScale from native height + env, sets projectM's render size, builds the target.
    void applyRenderSize();
    // (Re)creates the offscreen render target for the current _ssWidth/_ssHeight.
    void ensureSupersampleTarget();

    bool _shuffle{true};

    // audio input device characteristics
    unsigned int _numAudioDevices{0};
    int _curAudioDevice{0}; // SDL's device indexes are 0-based, -1 means "system default"
    unsigned short _audioChannelsCount{0};
    SDL_AudioDeviceID _audioDeviceId{0};
    int _selectedAudioDevice{0};

    // Preference-ordered video source list (highest priority first).
    std::vector<std::string> _videoDevicePrefs;

    // Foreground-masking preference from config ("Video Mask"); $PROJECTM_VIDEO_MASK overrides.
    std::string _videoMaskPref;

    // ONNX person-seg model path ("Video Seg Model"); empty = default. $PROJECTM_SEG_MODEL overrides.
    std::string _segModelPath;

    // Optional 2nd seg model ("Video Seg Model 2"); matte multiplies the primary's. $PROJECTM_SEG_MODEL2 overrides.
    std::string _segModelPath2;

    // How the 2nd model combines ("Video Seg Combine"): "multiply"/"gate". $PROJECTM_SEG_COMBINE overrides.
    std::string _segCombine;

    // ONNX person-seg quality level ("Video Seg Quality"): 1/2/3 -> 256/384/512; 0 = unset (2).
    int _segQuality{0};

    // Optional monocular depth model ("Video Seg Depth Model") to drop background people from the
    // matte; empty = off. $PROJECTM_SEG_DEPTH_MODEL overrides.
    std::string _segDepthModel;

    // Depth keep band ("Video Seg Depth Band", 0..1); <=0 = unset (default 0.20). $PROJECTM_SEG_DEPTH_BAND overrides.
    double _segDepthBand{0.0};

    // Matte-hardening smoothstep edges ("Video Seg Harden Lo/Hi"); lo<=0 && hi>=1 = off. $PROJECTM_SEG_HARDEN_LO/_HI override.
    double _segHardenLo{0.0};
    double _segHardenHi{1.0};

    // ONNX body-pose model path ("Video Pose Model"); empty = pose off. $PROJECTM_POSE_MODEL overrides.
    std::string _poseModelPath;

    // Whether the camera feed is mirrored ("Video Mirror"); the pose->touch bridge matches the flip.
    bool _videoMirror{false};

    std::string _presetName; //!< Current preset name

    // Frame-rate tracking: counts rendered frames over a ~1s wall-clock window and logs the
    // achieved rate (vs the configured target) once per second, tagged with the current preset.
    std::chrono::steady_clock::time_point _fpsWindowStart{}; //!< Start of the current measurement window.
    int _fpsFrameCount{0};                                   //!< Frames rendered in the current window.
    double _fpsFrameMsAccum{0.0};                            //!< Accumulated per-frame render time (ms) in the window.
    bool _fpsTrackerInitialized{false};                      //!< False until the first frame seeds the window start.

#ifdef PROJECTM_VIDEO_CAPTURE_ENABLED
    std::unique_ptr<VideoCapture> _videoCapture;
    std::unique_ptr<DepthCapture> _depthCapture; //!< Luxonis OAK depth-camera backend (when selected).
    std::unique_ptr<SegMasker> _segMasker;       //!< ONNX person-segmentation backend (when selected).
    std::unique_ptr<PoseTracker> _poseTracker;   //!< ONNX body-pose backend (when pose→touch is enabled).
    std::unique_ptr<PoseTouchBridge> _poseBridge; //!< Arbitrates pose hands into one touch stream.

    // Hand observations produced on the capture thread and consumed by drainPoseTouch() on the
    // main/render thread (projectm_touch* is not thread-safe, so the bridge runs main-thread only).
    std::mutex _poseMutex;
    std::vector<HandObservation> _poseHands;
    bool _poseFresh{false};
    std::chrono::steady_clock::time_point _poseLastDrain{};
    bool _poseDrainInit{false};
#endif

    //! Runs the pose->touch bridge from the latest capture-thread hand observations and applies the
    //! resulting touch command via projectm_touch*. Called on the main thread from renderFrame().
    void drainPoseTouch();
};
