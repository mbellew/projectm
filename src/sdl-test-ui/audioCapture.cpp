#include "audioCapture.hpp"
#include "pmSDL.hpp"

#include <algorithm>
#include <cctype>
#include <cstring>
#include <string>

// Case-insensitive "does haystack contain needle" used for matching capture
// device names against the user's preference list.
static bool nameContainsCI(const char* haystack, const std::string& needle)
{
    if (!haystack)
        return false;
    std::string h(haystack);
    std::string n(needle);
    auto toLower = [](unsigned char c) { return static_cast<char>(std::tolower(c)); };
    std::transform(h.begin(), h.end(), h.begin(), toLower);
    std::transform(n.begin(), n.end(), n.begin(), toLower);
    return h.find(n) != std::string::npos;
}


int projectMSDL::initAudioInput() {
    // params for audio input
    SDL_AudioSpec want, have;

    // requested format
    // https://wiki.libsdl.org/SDL_AudioSpec#Remarks
    SDL_zero(want);
    want.freq = 44100;
    want.format = AUDIO_F32;  // float
    want.channels = 2;  // mono might be better?
    want.samples = want.freq / 60;
    want.callback = projectMSDL::audioInputCallbackF32;
    want.userdata = this;

    // index -1 means "system default", which is used if we pass deviceName == NULL
    const char *deviceName = _selectedAudioDevice == -1 ? NULL : SDL_GetAudioDeviceName(_selectedAudioDevice, true);
    _audioDeviceId = SDL_OpenAudioDevice(deviceName, true, &want, &have, 0);

    if (_audioDeviceId == 0) {
        SDL_LogCritical(SDL_LOG_CATEGORY_APPLICATION, "Failed to open audio capture device: %s", SDL_GetError());
        return 0;
    }

    // read characteristics of opened capture device
    if(deviceName == NULL)
        deviceName = "<System default capture device>";
    SDL_Log("Opened audio capture device index=%i devId=%i: %s", _selectedAudioDevice, _audioDeviceId, deviceName);
    std::string deviceToast = deviceName; // Example: Microphone rear
    deviceToast += " selected";
#ifdef DEBUG
    SDL_Log("Samples: %i, frequency: %i, channels: %i, format: %i", have.samples, have.freq, have.channels, have.format);
#endif
    _audioChannelsCount = have.channels;

    return 1;
}

void projectMSDL::audioInputCallbackF32(void *userdata, unsigned char *stream, int len) {
    projectMSDL *app = (projectMSDL *) userdata;
//    printf("\nLEN: %i\n", len);
//    for (int i = 0; i < 64; i++)
//        printf("%X ", stream[i]);
    // stream is (i think) samples*channels floats (native byte order) of len BYTES
    if (app->_audioChannelsCount == 1)
        projectm_pcm_add_float(app->_projectM, reinterpret_cast<float*>(stream), len/sizeof(float)/2, PROJECTM_MONO);
    else if (app->_audioChannelsCount == 2)
        projectm_pcm_add_float(app->_projectM, reinterpret_cast<float*>(stream), len/sizeof(float)/2, PROJECTM_STEREO);
    else {
        SDL_LogCritical(SDL_LOG_CATEGORY_APPLICATION, "Multichannel audio not supported");
        SDL_Quit();
    }
}

int projectMSDL::toggleAudioInput() {
    // trigger a toggle with CMD-I or CTRL-I
    if (wasapi) { // we are currently on WASAPI, so we are going to revert to a microphone/line-in input.
        if (this->openAudioInput())
            this->beginAudioCapture();
        _curAudioDevice = -1;        // start from system default device
        _selectedAudioDevice = _curAudioDevice;
        this->wasapi = false; // Track wasapi as off so projectMSDL will stop listening to WASAPI loopback in pmSDL_main.
    }
    else {
        this->endAudioCapture(); // end current audio capture.
        _curAudioDevice++; // iterate device index
        if (_curAudioDevice >= (int) _numAudioDevices) { // We reached outside the boundaries of available audio devices.
            _curAudioDevice = -1; // Return to the default audio device.
#ifdef WASAPI_LOOPBACK
            // If we are at the boundary and WASAPI is enabled then let's load WASAPI instead.
            SDL_Log("Loopback audio selected");
            this->fakeAudio = false; // disable fakeAudio in case it was enabled.
            this->wasapi = true; // Track wasapi as on so projectMSDL will listen to it.
#else
            if (_numAudioDevices == 0) // If WASAPI_LOOPBACK was not enabled and there is only the default audio device, it's pointless to toggle anything.
            {
                SDL_Log("Only the default audio capture device is available. There is nothing to toggle at this time.");
                return 1;
            }
            // If WASAPI_LOOPBACK is not enabled and we have multiple input devices, return to device index 0 and let's listen to that device.
            _selectedAudioDevice = _curAudioDevice;
            initAudioInput();
            this->beginAudioCapture();
#endif
        }
        else {
            // This is a normal scenario where we move forward in the audio device index.
            _selectedAudioDevice = _curAudioDevice;
            initAudioInput();
            this->beginAudioCapture();
        }
    }
    return 1;
}

int projectMSDL::openAudioInput(const char* deviceName) {
    // Single-name convenience wrapper around the preference-list variant.
    std::vector<std::string> prefs;
    if (deviceName && deviceName[0])
        prefs.emplace_back(deviceName);
    return openAudioInput(prefs);
}

int projectMSDL::openAudioInput(const std::vector<std::string>& preferredNames) {
    fakeAudio = false; // if we are opening an audio input then there is no need for fake audio.
    // get audio driver name (static)
#ifdef DEBUG
    const char* driver_name = SDL_GetCurrentAudioDriver();
    SDL_Log("Using audio driver: %s\n", driver_name);
#endif

    // get audio input device
    _numAudioDevices = SDL_GetNumAudioDevices(true);  // capture, please

#ifdef DEBUG
    for (unsigned int i = 0; i < _numAudioDevices; i++) {
        SDL_Log("Found audio capture device %d: %s", i, SDL_GetAudioDeviceName(i, true));
    }
#endif

    // Walk the preference list in order; the first capture device whose name contains a
    // preferred substring (case-insensitive) wins. -1 means "system default".
    int initialDevice = -1;
    for (const auto& pref : preferredNames) {
        if (pref.empty())
            continue;
        for (unsigned int i = 0; i < _numAudioDevices; i++) {
            const char* name = SDL_GetAudioDeviceName(i, true);
            if (nameContainsCI(name, pref)) {
                initialDevice = static_cast<int>(i);
                SDL_LogInfo(SDL_LOG_CATEGORY_APPLICATION, "Selected audio device '%s' (matched '%s') at index %d", name, pref.c_str(), initialDevice);
                break;
            }
        }
        if (initialDevice != -1)
            break;
    }
    if (initialDevice == -1 && !preferredNames.empty()) {
        SDL_LogWarn(SDL_LOG_CATEGORY_APPLICATION, "No preferred audio device matched; falling back to default");
    }

    // We start with the system default capture device (index -1) unless overridden by name.
    // Note: the default might work even if NumAudioDevices == 0 (example: if only a
    // monitor device exists, and SDL_HINT_AUDIO_INCLUDE_MONITORS is not set).
    // So we always try it, and revert to fakeAudio if the default fails _and_ NumAudioDevices == 0.
    _curAudioDevice = initialDevice;
    _selectedAudioDevice = initialDevice;
    if(!initAudioInput() && _numAudioDevices == 0) {
        // the default device doesn't work, and there's no other device to try
        SDL_LogCritical(SDL_LOG_CATEGORY_APPLICATION, "No audio capture devices found");
        fakeAudio = true;
        return 0;
    }

    return 1;
}

void projectMSDL::beginAudioCapture() {
    // allocate a buffer to store PCM data for feeding in
    SDL_PauseAudioDevice(_audioDeviceId, false);
}

void projectMSDL::endAudioCapture() {
    SDL_PauseAudioDevice(_audioDeviceId, true);
    SDL_CloseAudioDevice(_audioDeviceId);
}

