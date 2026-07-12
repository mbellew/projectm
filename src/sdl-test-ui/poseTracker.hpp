/**
 * @file poseTracker.hpp
 * @brief ONNX 2D body-pose tracking: turns a color frame into per-person COCO-17
 *        keypoints, computed on the host (CoreML/ANE on macOS, CUDA on Linux).
 *
 * Runs a YOLO-pose model (yolo11n-pose / yolov8n-pose) on the same un-mirrored
 * camera frame the person-seg path uses, so keypoints share the matte's coordinate
 * space. Keypoints come out in normalized [0,1] coords with Y bottom-up, matching
 * the seg_ and touch_ variable convention. The pose-to-touch bridge consumes these;
 * the engine never sees pose, it only sees touch_ inputs move.
 *
 * Built against an installed onnxruntime when ENABLE_ONNX_SEG is on; otherwise a
 * stub (poseTracker_stub.cpp) provides the same interface and reports unsupported.
 */
#pragma once

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

//! COCO-17 keypoint indices (the ordering YOLO-pose emits).
enum class Kpt : int
{
    Nose = 0,
    LeftEye = 1,
    RightEye = 2,
    LeftEar = 3,
    RightEar = 4,
    LeftShoulder = 5,
    RightShoulder = 6,
    LeftElbow = 7,
    RightElbow = 8,
    LeftWrist = 9,
    RightWrist = 10,
    LeftHip = 11,
    RightHip = 12,
    LeftKnee = 13,
    RightKnee = 14,
    LeftAnkle = 15,
    RightAnkle = 16,
};

inline constexpr int kKeypointCount = 17;

struct Keypoint
{
    float x{0.0f};    //!< Normalized [0,1], left to right.
    float y{0.0f};    //!< Normalized [0,1], bottom to top (matches seg_*/touch_*).
    float conf{0.0f}; //!< Keypoint confidence, [0,1].
};

struct PersonPose
{
    float boxX0{0.0f}, boxY0{0.0f}, boxX1{0.0f}, boxY1{0.0f}; //!< Normalized bbox, y bottom-up.
    float score{0.0f};                                        //!< Person detection confidence, [0,1].
    Keypoint kpts[kKeypointCount]{};

    const Keypoint& operator[](Kpt k) const { return kpts[static_cast<int>(k)]; }
};

class PoseTracker
{
public:
    PoseTracker();
    ~PoseTracker();

    PoseTracker(const PoseTracker&) = delete;
    PoseTracker& operator=(const PoseTracker&) = delete;

    /** True when built against onnxruntime (a real backend exists). */
    static bool IsSupported();

    /**
     * Loads a YOLO-pose ONNX model.
     * @param size Square processing size for dynamic-input models; <=0 = default (640).
     *        $PROJECTM_POSE_SIZE overrides. Fixed-input models use their own size.
     * @return true on success.
     */
    bool Load(const std::string& modelPath, int size = 0);

    bool IsLoaded() const;

    /**
     * Runs pose on a BGRA/BGRX color frame, appending detected people (normalized
     * coords, y bottom-up) to @p out. @p out is cleared first.
     * @param mirror Horizontally flip to a selfie-style view (usually false; the seg
     *        path keeps the frame un-mirrored and the library/bridge applies the flip).
     */
    void Process(const uint8_t* bgra, int w, int h, bool mirror, std::vector<PersonPose>& out);

    /**
     * Same as Process(), but takes an already-converted interleaved RGB frame (w*h*3).
     *
     * The seg masker already builds exactly this buffer from the same BGRA frame every frame
     * (SegMasker::RgbFrame()); converting it a second time here was pure duplicated work at full
     * camera resolution. When seg is running, feed its buffer in instead.
     *
     * The frame must already be in the desired orientation (this does not mirror).
     */
    void ProcessRgb(const uint8_t* rgb, int w, int h, std::vector<PersonPose>& out);

private:
    struct Impl;
    std::unique_ptr<Impl> m_impl;
};
