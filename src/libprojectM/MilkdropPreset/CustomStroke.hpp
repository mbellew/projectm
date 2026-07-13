#pragma once

#include "Constants.hpp"
#include "PresetState.hpp"
#include "StrokePerFrameContext.hpp"

#include <Renderer/Mesh.hpp>

#include <projectm-eval.h>

namespace libprojectM {
namespace MilkdropPreset {

class PresetFileParser;

/**
 * @brief A cubic-Bezier ribbon: a stroke with a real, variable width.
 *
 * The gap this fills: custom shapes are regular N-gons, so drawing a curve with them means stamping
 * dozens of overlapping polygons along it -- expensive, and it cannot produce a continuously varying
 * stroke width. The outline path is worse: it is a GL LineLoop, and core-profile GL caps
 * glLineWidth at 1, so a shape's border is permanently a hairline.
 *
 * A stroke is defined by a cubic Bezier control net. The curve passes through (x0,y0) and (x3,y3);
 * (x1,y1) and (x2,y2) pull it without being touched -- a "loose spline". Width follows a quadratic
 * profile w0 -> wmid -> w1, so a stroke can taper to points at both ends (wmid > 0, w0 = w1 = 0) or
 * run at constant width (all three equal). Colour ramps from (r,g,b,a) at the start to
 * (r2,g2,b2,a2) at the end.
 *
 * It is tessellated into a single triangle strip, so the whole curve is ONE draw call.
 *
 * Preset keys: strokecode_N_* for the initial values, stroke_N_per_frame_N= for the code. Like
 * shapes, it is drawn into the warp/feedback buffer, so strokes persist and get warped.
 */
class CustomStroke
{
public:
    explicit CustomStroke(PresetState& presetState);

    virtual ~CustomStroke() = default;

    /**
     * @brief Loads the initial values and code from the preset file.
     * @param parsedFile The file parser with the preset data.
     * @param index The stroke index.
     */
    void Initialize(PresetFileParser& parsedFile, int index);

    /**
     * @brief Compiles the code blocks and runs the init expression.
     * @throws MilkdropCompileException Thrown if a code block couldn't be compiled.
     */
    void CompileCodeAndRunInitExpressions();

    /** @brief Renders the stroke. */
    void Draw();

private:
    Renderer::Mesh m_mesh; //!< Triangle strip: two vertices per sample along the curve.

    int m_index{0};        //!< The custom stroke index in the preset.
    bool m_enabled{false}; //!< If false, the stroke isn't drawn.
    bool m_additive{false};//!< Additive blending instead of alpha blending.
    int m_instances{1};    //!< Number of stroke instances to render.
    int m_segments{32};    //!< Tessellation steps along the curve.

    // Cubic-Bezier control net, in preset coordinates (0..1).
    float m_x0{0.25f}, m_y0{0.25f};
    float m_x1{0.25f}, m_y1{0.75f};
    float m_x2{0.75f}, m_y2{0.75f};
    float m_x3{0.75f}, m_y3{0.25f};

    // Width profile (quadratic Bezier w0 -> wmid -> w1). Default: tapers to points at both ends.
    float m_w0{0.0f}, m_wmid{0.03f}, m_w1{0.0f};

    // Colour ramp, start -> end.
    float m_r{1.0f}, m_g{1.0f}, m_b{1.0f}, m_a{1.0f};
    float m_r2{1.0f}, m_g2{1.0f}, m_b2{1.0f}, m_a2{1.0f};

    PresetState& m_presetState;                 //!< The preset state this stroke belongs to.
    StrokePerFrameContext m_perFrameContext;    //!< Per-frame evaluation code context.

    std::string m_perFrameInitCode;             //!< Code executed once at preset load.
    std::string m_perFrameCode;                 //!< Code executed once per instance per frame.

    PRJM_EVAL_F m_tValuesAfterInitCode[TVarCount]{}; //!< t1..t8 as left by the init code.

    friend class StrokePerFrameContext;
};

} // namespace MilkdropPreset
} // namespace libprojectM
