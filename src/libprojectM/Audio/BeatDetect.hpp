/**
 * @file BeatDetect.hpp
 * @brief Frame-rate-independent beat tracker: a phase-locked beat phase + per-beat onset.
 *
 * Audio is pushed sample-by-sample into a mono ring (independent of render FPS). Each render
 * frame, the ring is drained in fixed STFT hops to build a continuous spectral-flux onset
 * envelope; tempo is estimated by autocorrelation and a phase oscillator (PLL) entrains to the
 * beat. Outputs a continuous phase in [0, 2*pi) and a boolean "a beat landed this frame".
 *
 * See the project beat-detection design notes (decoupled window/hop, ring sizing).
 */
#pragma once

#include "Audio/MilkdropFFT.hpp"

#include <array>
#include <atomic>
#include <cstdint>
#include <vector>

namespace libprojectM {
namespace Audio {

class BeatDetect
{
public:
    BeatDetect();

    /** @brief Audio thread: append one mono sample to the ring. Lock-free (single producer). */
    void Push(float monoSample);

    /** @brief Render thread: drain the ring and advance the tracker. Call once per frame. */
    void Update(double secondsSinceLastFrame);

    /** @brief Beat phase, 0 .. 2*pi, phase-locked to the music (0 = on the beat). */
    auto Phase() const -> double { return m_phase; }

    /** @brief True on frames where a beat landed (the phase wrapped past 2*pi). */
    auto Onset() const -> bool { return m_beatThisFrame; }

    /** @brief Estimated tempo in BPM (0 until locked; needs the sample-rate estimate). */
    auto Bpm() const -> double { return m_bpm; }

    /** @brief Tracker confidence in [0,1] (autocorrelation peak salience). */
    auto Confidence() const -> double { return m_confidence; }

private:
    void ProcessHop();      //!< One STFT hop -> one envelope sample -> one tracker step.
    void EstimateTempo();   //!< Autocorrelation of the onset envelope -> beat period.
    void TrackerStep(float flux); //!< Advance the phase oscillator, onset-correct (PLL).

    // Fixed STFT geometry (samples). 1024/256 ~= 23 ms window / 6 ms hop at 44.1 kHz; window
    // gives ~43 Hz bins (resolves the kick), 75% overlap gives ~6 ms onset timing.
    static constexpr int kRing = 8192;    //!< Mono input ring (power of two). ~85 ms @96k -> survives low FPS.
    static constexpr int kWindow = 1024;  //!< FFT window size.
    static constexpr int kHop = 256;      //!< Hop between windows (envelope sample period).
    static constexpr int kBins = 512;     //!< FFT magnitude bins (= kWindow / 2).
    static constexpr int kEnvLen = 1024;  //!< Onset-envelope history (~6 s @44.1k) for autocorrelation.
    static constexpr double kMinBpm = 60.0;
    static constexpr double kMaxBpm = 180.0;
    static constexpr double kPriorBpm = 120.0; //!< Perceptual tempo prior (fixes half/double).

    // Input ring (single-producer / single-consumer). m_writePos is the synchronization point.
    std::array<float, kRing> m_ring{};
    std::atomic<uint64_t> m_writePos{0}; //!< Total samples ever written (audio thread).
    uint64_t m_lastWritePos{0};          //!< Snapshot from the previous frame (sample-rate estimate).
    uint64_t m_nextHopEnd{kWindow};      //!< Absolute sample index where the next hop window ends.

    double m_sampleRate{0.0};            //!< Estimated input sample rate (samples added / elapsed time).

    // Onset analysis.
    MilkdropFFT m_fft{kWindow, kBins, false, 1.0f}; //!< Windowed FFT, no equalization.
    std::vector<float> m_windowBuf;      //!< Scratch: current hop's time-domain window.
    std::vector<float> m_mag;            //!< Scratch: current magnitude spectrum.
    std::array<float, kBins> m_prevMag{};//!< Previous magnitude spectrum (for spectral flux).

    std::array<float, kEnvLen> m_env{};  //!< Onset-strength envelope ring.
    std::array<float, kEnvLen> m_envLinear{}; //!< Scratch: envelope oldest->newest for autocorrelation.
    uint64_t m_envWrite{0};              //!< Total envelope samples written.

    // Tracker state.
    double m_phase{0.0};                 //!< Beat phase, 0 .. 2*pi.
    double m_periodHops{0.0};            //!< Beat period in envelope hops (0 = not yet seeded).
    double m_bpm{0.0};
    double m_confidence{0.0};
    bool m_beatThisFrame{false};

    float m_fluxAvg{0.0f};               //!< Slow average of flux (adaptive onset threshold).
    float m_prevFlux{0.0f};
    int m_sinceOnset{1000};              //!< Hops since the last accepted onset (refractory).
};

} // namespace Audio
} // namespace libprojectM
