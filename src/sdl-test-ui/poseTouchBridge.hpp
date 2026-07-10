/**
 * @file poseTouchBridge.hpp
 * @brief Turns per-frame hand observations into a single arbitrated touch stream.
 *
 * Pure logic, no ONNX and no projectM dependency: it consumes HandObservations
 * (already in normalized [0,1], y bottom-up coords, with depth/confidence sampled
 * by the caller) and returns one TouchCommand per tick. The caller feeds the
 * command to projectm_touch/_drag/_destroy on the main thread.
 *
 * Pipeline (see POSE_TOUCH.md):
 *   per-hand score -> per-hand activation gate (hysteresis + dwell)
 *   -> sticky-owner arbitration (switch margin + dwell) -> one-euro smoothing
 *   -> emission state machine.
 *
 * Dwell/margin thresholds are expressed in frames-at-refFps but applied as time
 * (dt seconds), so behavior is frame-rate independent.
 */
#pragma once

#include <vector>

//! Live-tunable parameters. Constructed from defaults; ReadEnvOverrides() applies
//! PROJECTM_POSE_* env vars for live tuning without a rebuild.
struct PoseTouchParams
{
    // Score weights (conf / motion / depth / raised).
    float wConf{0.30f};
    float wMotion{0.40f};
    float wDepth{0.20f};
    float wRaised{0.10f};

    // Activation gate: enter ACTIVE above sOn for tOn frames; leave below sOff for tOff frames.
    // Tuned live (2026-07-10): a still raised hand scores ~0.40, a waving hand 0.5-0.9, so sOn=0.50
    // separates them and sOff=0.30 keeps a briefly-paused waving hand latched.
    float sOn{0.50f};
    float sOff{0.30f};
    float tOn{4.0f};
    float tOff{6.0f};

    // Sticky-owner arbitration: a challenger must beat the owner by switchMargin for tSwitch frames.
    float switchMargin{0.15f};
    float tSwitch{5.0f};

    // One-euro filter on the emitted owner position.
    float oneEuroMinCutoff{1.0f};
    float oneEuroBeta{0.01f};
    float oneEuroDCutoff{1.0f};

    // Wrist speed (screen-fractions/sec) that maps to motion=1.0 before clamping.
    float motionScale{1.0f};

    // Frame-to-frame hand association gate (normalized distance) and how long a track survives
    // while unseen (frames). Tuned up from the spec: a fast wave drops keypoint confidence
    // intermittently, so a wider gate + longer memory keep one physical hand on one track id
    // instead of churning (which caused the touch point to blink).
    float assocRadius{0.30f};
    float maxMisses{25.0f};

    // Handoff on owner switch: false = jump (reset the smoother so the point snaps to the new
    // hand), true = drag (carry the smoother so the point glides across). Default drag: residual
    // track re-identification of a fast hand causes occasional owner hops, and gliding reads far
    // better than snapping through them.
    bool handoffDrag{true};

    // Frame rate the frame-count thresholds above assume; dt scales them to real time.
    float refFps{30.0f};

    //! Overlay PROJECTM_POSE_* env vars onto these values (called once at construction).
    void ReadEnvOverrides();
};

//! One candidate hand for a single tick, in the bridge's coordinate frame (normalized,
//! y bottom-up; the caller applies any mirror before filling this).
struct HandObservation
{
    float x{0.5f};      //!< Wrist X, [0,1].
    float y{0.5f};      //!< Wrist Y, [0,1] bottom-up.
    float conf{0.0f};   //!< Wrist confidence, [0,1] (caller may fold in matte-alpha).
    float depth{-1.0f}; //!< Closeness, [0,1] (1 = nearest); <0 if unavailable.
    float raised{0.0f}; //!< Gesture-raised metric, [0,1] (wrist above shoulder => higher).
};

enum class TouchOp
{
    None,  //!< Do nothing this tick.
    Down,  //!< Begin/replace the touch point (projectm_touch).
    Drag,  //!< Move the active touch point (projectm_touch_drag).
    Up,    //!< End the touch (projectm_touch_destroy).
};

struct TouchCommand
{
    TouchOp op{TouchOp::None};
    float x{0.5f};
    float y{0.5f};
    float pressure{0.0f}; //!< [0,1]; caller converts for the int projectm_touch pressure arg.
};

class PoseTouchBridge
{
public:
    explicit PoseTouchBridge(const PoseTouchParams& params = PoseTouchParams{});

    //! Advance one tick with all candidate hands seen this frame; returns the touch command
    //! to apply. @p dt is seconds since the previous Update.
    TouchCommand Update(const std::vector<HandObservation>& hands, float dt);

    // --- Introspection (for debug logging) ---
    int OwnerId() const { return m_ownerId; }
    int TrackCount() const { return static_cast<int>(m_tracks.size()); }
    int ActiveCount() const;
    float OwnerScore() const;
    bool Touching() const { return m_touching; }

    struct TrackInfo
    {
        int id{0};
        float x{0.0f};
        float y{0.0f};
        float score{0.0f};
        float speed{0.0f};
        bool active{false};
        bool owner{false};
    };
    //! Snapshot of all live tracks (for temporary tuning/debug logging).
    std::vector<TrackInfo> DebugTracks() const;

private:
    struct OneEuro
    {
        float minCutoff{1.0f};
        float beta{0.0f};
        float dCutoff{1.0f};
        bool initialized{false};
        float xPrev{0.0f};
        float dxPrev{0.0f};

        void Reset() { initialized = false; }
        float Filter(float x, float dt);
    };

    struct Track
    {
        int id{0};
        float x{0.5f};
        float y{0.5f};
        float depth{-1.0f}; //!< Latest closeness from the observation ([0,1], <0 = none).
        float speed{0.0f};  //!< Smoothed wrist speed, frac/sec.
        float score{0.0f};
        float onTimer{0.0f};
        float offTimer{0.0f};
        bool active{false};
        int misses{0};
        bool seen{false};
    };

    float ScoreFor(const HandObservation& h, float motionNorm) const;

    PoseTouchParams m_params;
    std::vector<Track> m_tracks;
    int m_nextId{1};
    int m_ownerId{-1};
    float m_switchTimer{0.0f};
    bool m_touching{false};
    OneEuro m_filterX;
    OneEuro m_filterY;
};
