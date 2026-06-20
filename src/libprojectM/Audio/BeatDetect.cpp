#include "Audio/BeatDetect.hpp"

#include <algorithm>
#include <cmath>

namespace libprojectM {
namespace Audio {

namespace {
constexpr double kTwoPi = 6.283185307179586;
constexpr double kPi = 3.141592653589793;

// Tracker tuning.
constexpr float kOnsetFactor = 1.6f;   //!< Flux must exceed this * slow-average to count as an onset.
constexpr double kPhaseGain = 0.10;    //!< PLL correction strength (phase nudge toward the onset).
constexpr double kPeriodSmooth = 0.10; //!< EMA factor when updating the beat period.

auto WrapToPi(double angle) -> double
{
    while (angle > kPi) { angle -= kTwoPi; }
    while (angle < -kPi) { angle += kTwoPi; }
    return angle;
}
} // namespace

BeatDetect::BeatDetect()
{
    m_windowBuf.assign(kWindow, 0.0f);
    m_mag.assign(kBins, 0.0f);
}

void BeatDetect::Push(float monoSample)
{
    const uint64_t pos = m_writePos.load(std::memory_order_relaxed);
    m_ring[pos % kRing] = monoSample;
    // Release so the slot write is visible before the consumer sees the advanced count.
    m_writePos.store(pos + 1, std::memory_order_release);
}

void BeatDetect::Update(double secondsSinceLastFrame)
{
    const uint64_t writePos = m_writePos.load(std::memory_order_acquire);

    // Estimate the input sample rate from how many samples arrived this frame. Lets the tempo
    // autocorrelation map BPM <-> hop-lags without the app declaring a rate.
    const uint64_t added = writePos - m_lastWritePos;
    m_lastWritePos = writePos;
    if (secondsSinceLastFrame > 1e-4 && added > 0)
    {
        const double rate = static_cast<double>(added) / secondsSinceLastFrame;
        if (rate > 8000.0 && rate < 250000.0)
        {
            m_sampleRate = (m_sampleRate <= 0.0) ? rate : m_sampleRate * 0.9 + rate * 0.1;
            if (m_periodHops <= 0.0)
            {
                // Seed the period at the perceptual prior so phase advances before tempo locks.
                m_periodHops = (m_sampleRate / kHop) * 60.0 / kPriorBpm;
            }
        }
    }

    m_beatThisFrame = false;

    // Drain every hop whose window is fully available (catch-up on slow frames; nothing on fast
    // ones). This keeps the envelope continuous and gap-free regardless of render FPS.
    while (m_nextHopEnd <= writePos && (m_nextHopEnd - kWindow) + kRing >= writePos)
    {
        ProcessHop();
        m_nextHopEnd += kHop;
    }
    // If we fell so far behind that the window start has aged out of the ring (a long stall),
    // skip forward to the newest fully-available window rather than reading stale/overwritten data.
    if (m_nextHopEnd <= writePos)
    {
        const uint64_t newest = writePos - ((writePos - m_nextHopEnd) % kHop);
        m_nextHopEnd = newest;
        while (m_nextHopEnd <= writePos)
        {
            ProcessHop();
            m_nextHopEnd += kHop;
        }
    }

    EstimateTempo();
}

void BeatDetect::ProcessHop()
{
    const uint64_t start = m_nextHopEnd - kWindow;
    for (int i = 0; i < kWindow; ++i)
    {
        m_windowBuf[i] = m_ring[(start + static_cast<uint64_t>(i)) % kRing];
    }

    m_fft.TimeToFrequencyDomain(m_windowBuf, m_mag);
    if (m_mag.size() < static_cast<size_t>(kBins))
    {
        return; // FFT not ready
    }

    // Spectral flux: total positive change in magnitude since the previous hop = onset strength.
    float flux = 0.0f;
    for (int k = 0; k < kBins; ++k)
    {
        const float diff = m_mag[k] - m_prevMag[k];
        if (diff > 0.0f)
        {
            flux += diff;
        }
        m_prevMag[k] = m_mag[k];
    }

    m_env[m_envWrite % kEnvLen] = flux;
    ++m_envWrite;

    TrackerStep(flux);
}

void BeatDetect::TrackerStep(float flux)
{
    // Adaptive onset detection: a flux peak well above its slow running average, rate-limited by a
    // refractory period (no two beats closer than ~a quarter of the current period).
    m_fluxAvg = m_fluxAvg * 0.99f + flux * 0.01f;
    ++m_sinceOnset;

    const int refractory = std::max(6, static_cast<int>(m_periodHops * 0.25));
    bool onset = false;
    float onsetStrength = 0.0f;
    if (flux > m_fluxAvg * kOnsetFactor && flux >= m_prevFlux && m_sinceOnset >= refractory)
    {
        onset = true;
        onsetStrength = (m_fluxAvg > 1e-6f) ? (flux / m_fluxAvg) : 1.0f;
        m_sinceOnset = 0;
    }
    m_prevFlux = flux;

    if (m_periodHops <= 1.0)
    {
        return; // no tempo yet
    }

    // Advance the phase oscillator one hop.
    const double dphase = kTwoPi / m_periodHops;
    m_phase += dphase;

    // PLL: at an onset near a beat, pull the phase toward that beat (the onset should sit at
    // phase 0). Onsets far from a beat (near phase pi, i.e. off-beat subdivisions) are ignored so
    // we lock to the main pulse, not its subdivisions. Stronger onsets correct a touch harder.
    if (onset)
    {
        const double err = WrapToPi(m_phase); // distance to the nearest beat
        if (std::fabs(err) < kPi * 0.5)
        {
            const double weight = std::min(1.0, 0.5 + 0.1 * onsetStrength);
            m_phase -= kPhaseGain * weight * err;
        }
    }

    // A beat lands when the phase completes a cycle.
    if (m_phase >= kTwoPi)
    {
        m_phase -= kTwoPi;
        m_beatThisFrame = true;
    }
    while (m_phase < 0.0)
    {
        m_phase += kTwoPi;
    }
}

void BeatDetect::EstimateTempo()
{
    if (m_sampleRate <= 0.0 || m_envWrite < static_cast<uint64_t>(kEnvLen))
    {
        return; // need a full envelope history and a rate estimate
    }

    const double envRate = m_sampleRate / kHop; // onset-envelope samples per second
    int minLag = static_cast<int>(envRate * 60.0 / kMaxBpm);
    int maxLag = static_cast<int>(envRate * 60.0 / kMinBpm);
    minLag = std::max(minLag, 2);
    maxLag = std::min(maxLag, kEnvLen - 2);
    if (maxLag <= minLag)
    {
        return;
    }

    // Linearize the envelope ring (oldest -> newest) and mean-subtract so DC doesn't dominate.
    const uint64_t oldest = m_envWrite - static_cast<uint64_t>(kEnvLen);
    double mean = 0.0;
    for (int j = 0; j < kEnvLen; ++j)
    {
        const float value = m_env[(oldest + static_cast<uint64_t>(j)) % kEnvLen];
        m_envLinear[j] = value;
        mean += value;
    }
    mean /= static_cast<double>(kEnvLen);
    for (int j = 0; j < kEnvLen; ++j)
    {
        m_envLinear[j] -= static_cast<float>(mean);
    }

    // Autocorrelation over the musical lag range, weighted by a log-BPM Gaussian prior (pulls the
    // pick toward ~120 BPM, resolving half/double-tempo ambiguity).
    double zeroLag = 1e-9;
    for (int j = 0; j < kEnvLen; ++j)
    {
        zeroLag += static_cast<double>(m_envLinear[j]) * m_envLinear[j];
    }

    double bestScore = -1.0;
    double bestNorm = 0.0;
    int bestLag = 0;
    for (int lag = minLag; lag <= maxLag; ++lag)
    {
        double sum = 0.0;
        for (int j = 0; j + lag < kEnvLen; ++j)
        {
            sum += static_cast<double>(m_envLinear[j]) * m_envLinear[j + lag];
        }
        const double norm = sum / zeroLag; // normalized autocorrelation, ~[0,1]
        const double bpm = 60.0 * envRate / lag;
        const double logRatio = std::log2(bpm / kPriorBpm) / 0.7;
        const double weight = std::exp(-0.5 * logRatio * logRatio);
        const double score = norm * weight;
        if (score > bestScore)
        {
            bestScore = score;
            bestNorm = norm;
            bestLag = lag;
        }
    }

    if (bestLag <= 0)
    {
        return;
    }

    m_confidence = std::clamp(bestNorm, 0.0, 1.0);

    // Only retune when the lock is reasonably confident; otherwise free-wheel at the last period.
    if (m_confidence > 0.10)
    {
        m_periodHops = (m_periodHops <= 0.0)
                           ? static_cast<double>(bestLag)
                           : m_periodHops * (1.0 - kPeriodSmooth) + static_cast<double>(bestLag) * kPeriodSmooth;
        m_bpm = 60.0 * envRate / m_periodHops;
    }
}

} // namespace Audio
} // namespace libprojectM
