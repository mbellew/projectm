/**
 * @file FrameAudioData.hpp
 * @brief Holds all audio data to be used to render a single frame.
 *
 * This includes the actual waveform data, spectrum and beat detection values.
 */
#pragma once

#include "Audio/AudioConstants.hpp"

#include <projectM-4/projectM_cxx_export.h>

#include <array>

namespace libprojectM {
namespace Audio {

class PROJECTM_CXX_EXPORT FrameAudioData
{
public:
    float bass{0.f};
    float bassAtt{0.f};
    float mid{0.f};
    float midAtt{0.f};
    float treb{0.f};
    float trebAtt{0.f};

    float vol{0.f};
    float volAtt{0.f};

    float beatPhase{0.f}; //!< Phase-locked beat phase, 0 .. 2*pi (0 = on the beat).
    float beatOnset{0.f}; //!< 1.0 on frames where a beat landed, else 0.0.
    float bpm{0.f};       //!< Estimated tempo in BPM (0 until the tracker locks).
    float beatConf{0.f};  //!< Beat-tracker confidence, 0 .. 1.

    std::array<float, WaveformSamples> waveformLeft;
    std::array<float, WaveformSamples> waveformRight;

    std::array<float, SpectrumSamples> spectrumLeft;
    std::array<float, SpectrumSamples> spectrumRight;
};

} // namespace Audio
} // namespace libprojectM
