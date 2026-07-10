/**
 * @file poseTouchBridge.cpp
 * @brief Implementation of the pose-hand -> single-touch arbitration bridge.
 */
#include "poseTouchBridge.hpp"

#include <algorithm>
#include <cmath>
#include <cstdlib>

namespace {

constexpr float kPi = 3.14159265358979323846f;

float EnvF(const char* name, float fallback)
{
    const char* v = std::getenv(name);
    return (v && v[0]) ? static_cast<float>(std::atof(v)) : fallback;
}

float Clamp01(float v)
{
    return std::clamp(v, 0.0f, 1.0f);
}

} // namespace

void PoseTouchParams::ReadEnvOverrides()
{
    wConf = EnvF("PROJECTM_POSE_W_CONF", wConf);
    wMotion = EnvF("PROJECTM_POSE_W_MOTION", wMotion);
    wDepth = EnvF("PROJECTM_POSE_W_DEPTH", wDepth);
    wRaised = EnvF("PROJECTM_POSE_W_RAISED", wRaised);
    sOn = EnvF("PROJECTM_POSE_S_ON", sOn);
    sOff = EnvF("PROJECTM_POSE_S_OFF", sOff);
    tOn = EnvF("PROJECTM_POSE_T_ON", tOn);
    tOff = EnvF("PROJECTM_POSE_T_OFF", tOff);
    switchMargin = EnvF("PROJECTM_POSE_SWITCH_MARGIN", switchMargin);
    tSwitch = EnvF("PROJECTM_POSE_T_SWITCH", tSwitch);
    oneEuroMinCutoff = EnvF("PROJECTM_POSE_EURO_MINCUT", oneEuroMinCutoff);
    oneEuroBeta = EnvF("PROJECTM_POSE_EURO_BETA", oneEuroBeta);
    motionScale = EnvF("PROJECTM_POSE_MOTION_SCALE", motionScale);
    assocRadius = EnvF("PROJECTM_POSE_ASSOC_RADIUS", assocRadius);
    maxMisses = EnvF("PROJECTM_POSE_MAX_MISSES", maxMisses);
    handoffDrag = EnvF("PROJECTM_POSE_HANDOFF_DRAG", handoffDrag ? 1.0f : 0.0f) >= 0.5f;
}

float PoseTouchBridge::OneEuro::Filter(float x, float dt)
{
    if (!initialized || dt <= 0.0f)
    {
        initialized = true;
        xPrev = x;
        dxPrev = 0.0f;
        return x;
    }
    auto alpha = [dt](float cutoff) {
        const float tau = 1.0f / (2.0f * kPi * cutoff);
        return 1.0f / (1.0f + tau / dt);
    };
    const float dx = (x - xPrev) / dt;
    const float aD = alpha(dCutoff);
    dxPrev += aD * (dx - dxPrev);
    const float cutoff = minCutoff + beta * std::fabs(dxPrev);
    const float a = alpha(cutoff);
    xPrev += a * (x - xPrev);
    return xPrev;
}

PoseTouchBridge::PoseTouchBridge(const PoseTouchParams& params)
    : m_params(params)
{
    m_filterX.minCutoff = m_params.oneEuroMinCutoff;
    m_filterX.beta = m_params.oneEuroBeta;
    m_filterX.dCutoff = m_params.oneEuroDCutoff;
    m_filterY = m_filterX;
}

float PoseTouchBridge::ScoreFor(const HandObservation& h, float motionNorm) const
{
    const bool hasDepth = h.depth >= 0.0f;
    float wsum = m_params.wConf + m_params.wMotion + m_params.wRaised + (hasDepth ? m_params.wDepth : 0.0f);
    if (wsum <= 1e-6f)
    {
        return 0.0f;
    }
    float s = m_params.wConf * Clamp01(h.conf) + m_params.wMotion * Clamp01(motionNorm) +
              m_params.wRaised * Clamp01(h.raised);
    if (hasDepth)
    {
        s += m_params.wDepth * Clamp01(h.depth);
    }
    return s / wsum;
}

int PoseTouchBridge::ActiveCount() const
{
    int n = 0;
    for (const auto& t : m_tracks)
    {
        if (t.active) { ++n; }
    }
    return n;
}

std::vector<PoseTouchBridge::TrackInfo> PoseTouchBridge::DebugTracks() const
{
    std::vector<TrackInfo> out;
    out.reserve(m_tracks.size());
    for (const auto& t : m_tracks)
    {
        out.push_back(TrackInfo{t.id, t.x, t.y, t.score, t.speed, t.active, t.id == m_ownerId});
    }
    return out;
}

float PoseTouchBridge::OwnerScore() const
{
    for (const auto& t : m_tracks)
    {
        if (t.id == m_ownerId) { return t.score; }
    }
    return 0.0f;
}

TouchCommand PoseTouchBridge::Update(const std::vector<HandObservation>& hands, float dt)
{
    const float onDwell = m_params.tOn / std::max(1.0f, m_params.refFps);
    const float offDwell = m_params.tOff / std::max(1.0f, m_params.refFps);
    const float switchDwell = m_params.tSwitch / std::max(1.0f, m_params.refFps);

    for (auto& t : m_tracks) { t.seen = false; }

    // --- Association: match each observation to the nearest free track (greedy, high-conf first).
    std::vector<int> order(hands.size());
    for (size_t i = 0; i < hands.size(); ++i) { order[i] = static_cast<int>(i); }
    std::sort(order.begin(), order.end(),
              [&](int a, int b) { return hands[a].conf > hands[b].conf; });

    for (int oi : order)
    {
        const HandObservation& h = hands[oi];
        int best = -1;
        float bestD2 = m_params.assocRadius * m_params.assocRadius;
        for (size_t k = 0; k < m_tracks.size(); ++k)
        {
            if (m_tracks[k].seen) { continue; }
            const float dx = m_tracks[k].x - h.x;
            const float dy = m_tracks[k].y - h.y;
            const float d2 = dx * dx + dy * dy;
            if (d2 < bestD2) { bestD2 = d2; best = static_cast<int>(k); }
        }

        if (best >= 0)
        {
            Track& t = m_tracks[best];
            const float dist = std::sqrt((t.x - h.x) * (t.x - h.x) + (t.y - h.y) * (t.y - h.y));
            const float instSpeed = (dt > 1e-5f) ? dist / dt : 0.0f;
            const float av = (dt > 0.0f) ? 1.0f - std::exp(-dt / 0.12f) : 1.0f; // ~0.12s smoothing
            t.speed += (instSpeed - t.speed) * av;
            t.x = h.x;
            t.y = h.y;
            t.depth = h.depth;
            t.misses = 0;
            t.seen = true;
            t.score = ScoreFor(h, t.speed / std::max(1e-3f, m_params.motionScale));
        }
        else
        {
            Track t{};
            t.id = m_nextId++;
            t.x = h.x;
            t.y = h.y;
            t.depth = h.depth;
            t.speed = 0.0f;
            t.misses = 0;
            t.seen = true;
            t.score = ScoreFor(h, 0.0f);
            m_tracks.push_back(t);
        }
    }

    // --- Age unseen tracks; drive their gate toward inactive with score 0.
    for (auto& t : m_tracks)
    {
        if (!t.seen)
        {
            ++t.misses;
            t.score = 0.0f;
            t.speed = 0.0f;
        }

        // Activation gate (asymmetric hysteresis + dwell), applied to every track.
        if (!t.active)
        {
            if (t.score > m_params.sOn)
            {
                t.onTimer += dt;
                if (t.onTimer >= onDwell) { t.active = true; t.offTimer = 0.0f; }
            }
            else
            {
                t.onTimer = 0.0f;
            }
        }
        else
        {
            if (t.score < m_params.sOff)
            {
                t.offTimer += dt;
                if (t.offTimer >= offDwell) { t.active = false; t.onTimer = 0.0f; }
            }
            else
            {
                t.offTimer = 0.0f;
            }
        }
    }

    // Remove dead tracks (unseen too long). Never remove the owner while it still exists as a
    // track; a gone owner is handled by arbitration below.
    m_tracks.erase(std::remove_if(m_tracks.begin(), m_tracks.end(),
                                  [&](const Track& t) {
                                      return t.misses > static_cast<int>(m_params.maxMisses);
                                  }),
                   m_tracks.end());

    // --- Arbitration: sticky owner.
    const int prevOwnerId = m_ownerId;

    auto findTrack = [&](int id) -> Track* {
        for (auto& t : m_tracks)
        {
            if (t.id == id) { return &t; }
        }
        return nullptr;
    };

    Track* owner = findTrack(m_ownerId);
    const bool ownerValid = owner && owner->active;

    // Best active challenger (highest score, not the current owner).
    Track* challenger = nullptr;
    for (auto& t : m_tracks)
    {
        if (!t.active || t.id == m_ownerId) { continue; }
        if (!challenger || t.score > challenger->score) { challenger = &t; }
    }

    if (ownerValid)
    {
        if (challenger && challenger->score > owner->score + m_params.switchMargin)
        {
            m_switchTimer += dt;
            if (m_switchTimer >= switchDwell)
            {
                m_ownerId = challenger->id;
                m_switchTimer = 0.0f;
            }
        }
        else
        {
            m_switchTimer = 0.0f;
        }
    }
    else
    {
        // Owner gone or inactive: hand ownership to the best active track immediately (no margin).
        m_switchTimer = 0.0f;
        Track* best = nullptr;
        for (auto& t : m_tracks)
        {
            if (!t.active) { continue; }
            if (!best || t.score > best->score) { best = &t; }
        }
        m_ownerId = best ? best->id : -1;
    }

    // --- Emission state machine.
    const bool ownerChanged = (m_ownerId != prevOwnerId) && prevOwnerId != -1 && m_ownerId != -1;
    Track* cur = findTrack(m_ownerId);

    TouchCommand cmd;
    if (!cur)
    {
        if (m_touching)
        {
            cmd.op = TouchOp::Up;
            cmd.x = m_filterX.xPrev;
            cmd.y = m_filterY.xPrev;
            m_touching = false;
        }
        return cmd;
    }

    // Owner present. Under jump handoff, reset the smoother so the point snaps to the new hand.
    if (ownerChanged && !m_params.handoffDrag)
    {
        m_filterX.Reset();
        m_filterY.Reset();
    }
    cmd.x = m_filterX.Filter(cur->x, dt);
    cmd.y = m_filterY.Filter(cur->y, dt);
    // Pressure from the owner's closeness when depth is available, else 0 (no pressure axis).
    cmd.pressure = (cur->depth >= 0.0f) ? Clamp01(cur->depth) : 0.0f;

    if (!m_touching)
    {
        cmd.op = TouchOp::Down;
        m_touching = true;
    }
    else if (ownerChanged && !m_params.handoffDrag)
    {
        cmd.op = TouchOp::Down; // jump: re-plant the point on the new hand
    }
    else
    {
        cmd.op = TouchOp::Drag;
    }
    return cmd;
}
