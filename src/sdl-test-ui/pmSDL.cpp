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

#include <cstdlib>
#include <fstream>
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

    // Application-global foreground masking via $PROJECTM_VIDEO_MASK. The library does the
    // masking now (app-side pipeline retired); the app just selects the mode. Tokens map to
    // library alpha modes; append "-raw" to skip the refinement back-end (A/B comparison).
    //   off (default) | source | const | motion | decay | chroma | bgsub
    {
        int maskMode = -1; // -1 = preset-controlled
        bool refine = true;
        const char* maskEnv = std::getenv("PROJECTM_VIDEO_MASK");
        std::string maskStr = maskEnv ? std::string(maskEnv) : _videoMaskPref; // env overrides config
        if (!maskStr.empty())
        {
            std::string m = maskStr;
            if (m.size() > 4 && m.compare(m.size() - 4, 4, "-raw") == 0)
            {
                refine = false;
                m.erase(m.size() - 4);
            }
            if (m == "source") { maskMode = 0; }
            else if (m == "const" || m == "constant") { maskMode = 1; }
            else if (m == "motion") { maskMode = 2; }
            else if (m == "decay" || m == "motiondecay") { maskMode = 3; }
            else if (m == "chroma" || m == "chromakey") { maskMode = 4; }
            else if (m == "bgsub" || m == "bg" || m == "on" || m == "1") { maskMode = 5; }
        }
        projectm_video_set_mask_mode(_projectM, maskMode, refine);
        if (maskMode >= 0)
        {
            SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, "Video foreground masking: mode=%d refine=%d.",
                        maskMode, refine ? 1 : 0);
        }
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
        preferredDevices);

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
    if (_videoCapture)
    {
        _videoCapture->Stop();
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

    projectm_set_window_size(_projectM, _width, _height);
}

void projectMSDL::pollEvent()
{
    SDL_Event evt;

    int mousex = 0;
    float mousexscale = 0;
    int mousey = 0;
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
                        // Get mouse coorindates when you click.
                        SDL_GetMouseState(&mousex, &mousey);
                        // Scale those coordinates. libProjectM supports a scale of 0.1 instead of absolute pixel coordinates.
                        mousexscale = (mousex / (float) _width);
                        mouseyscale = ((_height - mousey) / (float) _height);
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

                    // Right Click
                    SDL_GetMouseState(&mousex, &mousey);

                    // Scale those coordinates. libProjectM supports a scale of 0.1 instead of absolute pixel coordinates.
                    mousexscale = (mousex / (float) _width);
                    mouseyscale = ((_height - mousey) / (float) _height);

                    // Destroy at the coordinates we clicked.
                    touchDestroy(mousexscale, mouseyscale);
                }
                break;

            case SDL_MOUSEBUTTONUP:
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
        // Get mouse coordinates when you click.
        SDL_GetMouseState(&mousex, &mousey);
        // Scale those coordinates. libProjectM supports a scale of 0.1 instead of absolute pixel coordinates.
        mousexscale = (mousex / (float) _width);
        mouseyscale = ((_height - mousey) / (float) _height);
        // Drag Touch.
        touchDrag(mousexscale, mouseyscale, mousepressure);
    }
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

void projectMSDL::renderFrame()
{
    const auto frameStart = std::chrono::steady_clock::now();

    glClearColor(0.0, 0.0, 0.0, 0.0);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    projectm_opengl_render_frame(_projectM);

    SDL_GL_SwapWindow(_sdlWindow);

    trackFrameRate(frameStart);
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
    projectm_set_window_size(_projectM, _width, _height);

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
