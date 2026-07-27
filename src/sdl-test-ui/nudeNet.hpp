/**
 * @file nudeNet.hpp
 * @brief ONNX NudeNet detector: exposure state of the MAIN figure as four de-flickered
 *        flags (top / rear / front-f / front-m) plus a female<->male face axis.
 *
 * Runs the NudeNet v3 YOLOv8 detector (320n.onnx, 18 classes) on the same un-mirrored
 * camera RGB frame the seg/pose paths use. Only detections overlapping the main person's
 * pose bounding box are counted, so background people don't trip it. The raw per-class
 * confidences are put through a Schmitt-trigger + glide so the flags are steady near-binary
 * values (covered/0 by default) rather than per-frame flicker.
 *
 * This is intentionally low-surface: four exposure flags + one gender axis, main-figure
 * only. See the class-index mapping in nudeNet.cpp. Built against onnxruntime when
 * ENABLE_ONNX_SEG is on; otherwise a stub (nudeNet_stub.cpp) reports unsupported.
 */
#pragma once

#include <cstdint>
#include <memory>
#include <string>

class NudeNet
{
public:
    NudeNet();
    ~NudeNet();

    NudeNet(const NudeNet&) = delete;
    NudeNet& operator=(const NudeNet&) = delete;

    /** True when built against onnxruntime (a real backend exists). */
    static bool IsSupported();

    /**
     * Loads a NudeNet detector ONNX model.
     * @param size Square processing size for dynamic-input models; <=0 = model/default (320).
     *        $PROJECTM_NUDENET_SIZE overrides. Fixed-input models use their own size.
     * @return true on success.
     */
    bool Load(const std::string& modelPath, int size = 0);

    bool IsLoaded() const;

    /**
     * Runs the detector on an interleaved RGB frame (w*h*3) and updates the internal
     * de-flickered verdict. Only detections whose box overlaps the main figure's
     * normalized bounding box (y bottom-up, matching pose/seg) are counted.
     *
     * @param personValid false when there is no tracked main figure this run; the verdict
     *        then decays toward 0 (covered) instead of holding stale exposure.
     *
     * Call this at a THROTTLED rate (a few Hz) -- the hysteresis integrates over calls, and
     * between calls the last Top()/Bottom()/Rear() simply persist.
     */
    void ProcessRgb(const uint8_t* rgb, int w, int h,
                    float boxX0, float boxY0, float boxX1, float boxY1, bool personValid);

    //! De-flickered exposure flags for the main figure, [0,1]; 0 = covered/none (the default).
    float Top() const;    //!< nude_top:     class 3  FEMALE_BREAST_EXPOSED.
    float Rear() const;   //!< nude_rear:    class 2  BUTTOCKS_EXPOSED.
    float FrontF() const; //!< nude_front_f: class 4  FEMALE_GENITALIA_EXPOSED.
    float FrontM() const; //!< nude_front_m: class 14 MALE_GENITALIA_EXPOSED.

    //! Female<->male FACE axis, [0,1]: 1 = female face, 0 = male face, 0.5 = no face / uncertain.
    float Female() const; //!< nude_female: from FACE_FEMALE (1) vs FACE_MALE (12).

    //! Breast-box centers from the last ProcessRgb (up to 2), NMS'd, gated to the main figure, for
    //! LOCATION refinement -- EXPOSED breasts only (covered boxes wander and would drag the good
    //! geometry when clothed). Normalized, un-mirrored, y bottom-up (same space as the pose joints).
    //! Count is per-run: 0 when nothing was detected this run. The app assigns them to L/R by proximity.
    int BreastCount() const;
    //! Center + detection score of breast box `index`. The score lets the caller weight the
    //! correction: strong (exposed) boxes are precise and should dominate; weak (covered) boxes
    //! wander and should defer to geometry.
    bool Breast(int index, float& x, float& y, float& score) const;

    //! Front-pelvis (groin) box center + score from the last run, for LOCATION refinement of the
    //! GROIN joint -- EXPOSED genitalia only (FEMALE 4 / MALE 14). One point, main-figure gated,
    //! normalized/un-mirrored/y-up. Returns false when nothing was detected this run.
    bool GroinBox(float& x, float& y, float& score) const;

private:
    struct Impl;
    std::unique_ptr<Impl> m_impl;
};
