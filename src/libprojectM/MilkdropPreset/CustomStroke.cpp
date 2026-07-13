#include "CustomStroke.hpp"

#include "PresetFileParser.hpp"

#include <Renderer/BlendMode.hpp>

#include <algorithm>
#include <cmath>

namespace libprojectM {
namespace MilkdropPreset {

CustomStroke::CustomStroke(PresetState& presetState)
    : m_mesh(Renderer::VertexBufferUsage::StreamDraw)
    , m_presetState(presetState)
    , m_perFrameContext(presetState.globalMemory, &presetState.globalRegisters,
                        &presetState.palette, &presetState.renderContext.pose)
{
    m_mesh.SetRenderPrimitiveType(Renderer::Mesh::PrimitiveType::TriangleStrip);
    m_mesh.SetUseColor(true);
    // Two vertices (one either side of the curve) per sample.
    m_mesh.SetVertexCount((MaxStrokeSegments + 1) * 2);
}

void CustomStroke::Initialize(PresetFileParser& parsedFile, int index)
{
    m_index = index;
    const auto prefix = "strokecode_" + std::to_string(index) + "_";

    m_enabled = parsedFile.GetBool(prefix + "enabled", m_enabled);
    m_additive = parsedFile.GetBool(prefix + "additive", m_additive);
    m_instances = parsedFile.GetInt(prefix + "num_inst", m_instances);
    m_segments = parsedFile.GetInt(prefix + "segments", m_segments);

    m_x0 = parsedFile.GetFloat(prefix + "x0", m_x0);
    m_y0 = parsedFile.GetFloat(prefix + "y0", m_y0);
    m_x1 = parsedFile.GetFloat(prefix + "x1", m_x1);
    m_y1 = parsedFile.GetFloat(prefix + "y1", m_y1);
    m_x2 = parsedFile.GetFloat(prefix + "x2", m_x2);
    m_y2 = parsedFile.GetFloat(prefix + "y2", m_y2);
    m_x3 = parsedFile.GetFloat(prefix + "x3", m_x3);
    m_y3 = parsedFile.GetFloat(prefix + "y3", m_y3);

    m_w0 = parsedFile.GetFloat(prefix + "w0", m_w0);
    m_wmid = parsedFile.GetFloat(prefix + "wmid", m_wmid);
    m_w1 = parsedFile.GetFloat(prefix + "w1", m_w1);

    m_r = parsedFile.GetFloat(prefix + "r", m_r);
    m_g = parsedFile.GetFloat(prefix + "g", m_g);
    m_b = parsedFile.GetFloat(prefix + "b", m_b);
    m_a = parsedFile.GetFloat(prefix + "a", m_a);
    m_r2 = parsedFile.GetFloat(prefix + "r2", m_r2);
    m_g2 = parsedFile.GetFloat(prefix + "g2", m_g2);
    m_b2 = parsedFile.GetFloat(prefix + "b2", m_b2);
    m_a2 = parsedFile.GetFloat(prefix + "a2", m_a2);

    m_perFrameInitCode = parsedFile.GetCode("stroke_" + std::to_string(index) + "_init");
    m_perFrameCode = parsedFile.GetCode("stroke_" + std::to_string(index) + "_per_frame");
}

void CustomStroke::CompileCodeAndRunInitExpressions()
{
    if (!m_enabled)
    {
        return;
    }

    m_perFrameContext.RegisterBuiltinVariables();
    m_perFrameContext.LoadStateVariables(m_presetState, *this, 0);
    m_perFrameContext.EvaluateInitCode(m_perFrameInitCode, *this);

    for (int t = 0; t < TVarCount; t++)
    {
        m_tValuesAfterInitCode[t] = *m_perFrameContext.t_vars[t];
    }

    m_perFrameContext.CompilePerFrameCode(m_perFrameCode, *this);
}

void CustomStroke::Draw()
{
    if (!m_enabled)
    {
        return;
    }

    auto shader = m_presetState.untexturedShader.lock();
    if (!shader)
    {
        return;
    }

    const int instances = std::clamp(m_instances, 1, 1024);

    // The X axis spans the wider side of the viewport, so a vector that looks perpendicular in NDC
    // is not perpendicular on screen. aspectY is the same correction CustomShape applies to keep its
    // polygons round (CustomShape.cpp:135) -- the ribbon's normals must use it or the stroke's width
    // would vary with its direction.
    const float aspectY = m_presetState.renderContext.aspectY;
    const float invAspectY = (aspectY > 1.0e-6f) ? (1.0f / aspectY) : 1.0f;

    auto& vertices = m_mesh.Vertices();
    auto& colors = m_mesh.Colors();

    for (int instance = 0; instance < instances; instance++)
    {
        m_perFrameContext.LoadStateVariables(m_presetState, *this, instance);
        m_perFrameContext.ExecutePerFrameCode();

        const int segments = std::clamp(static_cast<int>(*m_perFrameContext.segments), 2, MaxStrokeSegments);

        const double px[4] = {*m_perFrameContext.x0, *m_perFrameContext.x1,
                              *m_perFrameContext.x2, *m_perFrameContext.x3};
        const double py[4] = {*m_perFrameContext.y0, *m_perFrameContext.y1,
                              *m_perFrameContext.y2, *m_perFrameContext.y3};

        const double cw0 = *m_perFrameContext.w0;
        const double cwm = *m_perFrameContext.wmid;
        const double cw1 = *m_perFrameContext.w1;

        // Colour ramp endpoints as raw components: Renderer::Color keeps its channels private, so
        // interpolate here and construct the Color per sample.
        const float colStart[4] = {static_cast<float>(*m_perFrameContext.r),
                                   static_cast<float>(*m_perFrameContext.g),
                                   static_cast<float>(*m_perFrameContext.b),
                                   static_cast<float>(*m_perFrameContext.a)};
        const float colEnd[4] = {static_cast<float>(*m_perFrameContext.r2),
                                 static_cast<float>(*m_perFrameContext.g2),
                                 static_cast<float>(*m_perFrameContext.b2),
                                 static_cast<float>(*m_perFrameContext.a2)};

        // Carried across samples so a degenerate tangent (coincident control points) reuses the last
        // good normal instead of collapsing the ribbon.
        float prevNx = 0.0f;
        float prevNy = 1.0f;

        for (int i = 0; i <= segments; i++)
        {
            const double t = static_cast<double>(i) / static_cast<double>(segments);
            const double u = 1.0 - t;

            // Cubic Bezier position.
            const double b0 = u * u * u;
            const double b1 = 3.0 * u * u * t;
            const double b2 = 3.0 * u * t * t;
            const double b3 = t * t * t;
            const double cx = b0 * px[0] + b1 * px[1] + b2 * px[2] + b3 * px[3];
            const double cy = b0 * py[0] + b1 * py[1] + b2 * py[2] + b3 * py[3];

            // ...and its analytic derivative, for the tangent.
            const double d0 = 3.0 * u * u;
            const double d1 = 6.0 * u * t;
            const double d2 = 3.0 * t * t;
            const double dx = d0 * (px[1] - px[0]) + d1 * (px[2] - px[1]) + d2 * (px[3] - px[2]);
            const double dy = d0 * (py[1] - py[0]) + d1 * (py[2] - py[1]) + d2 * (py[3] - py[2]);

            // Tangent in NDC, then rescaled so its components are proportional to screen pixels.
            const float tx = static_cast<float>(dx * 2.0) * invAspectY;
            const float ty = static_cast<float>(dy * -2.0);
            const float len = std::sqrt(tx * tx + ty * ty);

            float nx = prevNx;
            float ny = prevNy;
            if (len > 1.0e-8f)
            {
                nx = -ty / len;
                ny = tx / len;
                prevNx = nx;
                prevNy = ny;
            }

            // Width profile: quadratic Bezier w0 -> wmid -> w1.
            const double width = u * u * cw0 + 2.0 * u * t * cwm + t * t * cw1;
            const float halfWidth = static_cast<float>(width) * 0.5f;

            // Back into NDC (X compressed by aspectY, exactly as CustomShape does).
            const float offsetX = halfWidth * nx * aspectY;
            const float offsetY = halfWidth * ny;

            const float ndcX = static_cast<float>(cx * 2.0 - 1.0);
            const float ndcY = static_cast<float>(cy * -2.0 + 1.0);

            vertices[i * 2 + 0] = Renderer::Point(ndcX + offsetX, ndcY + offsetY);
            vertices[i * 2 + 1] = Renderer::Point(ndcX - offsetX, ndcY - offsetY);

            const float ft = static_cast<float>(t);
            const auto color = Renderer::Color::Modulo(
                Renderer::Color(colStart[0] + (colEnd[0] - colStart[0]) * ft,
                                colStart[1] + (colEnd[1] - colStart[1]) * ft,
                                colStart[2] + (colEnd[2] - colStart[2]) * ft,
                                colStart[3] + (colEnd[3] - colStart[3]) * ft));
            colors[i * 2 + 0] = color;
            colors[i * 2 + 1] = color;
        }

        Renderer::BlendMode::SetBlendFunction(Renderer::BlendMode::Function::SourceAlpha,
                                              static_cast<int>(*m_perFrameContext.additive) != 0
                                                  ? Renderer::BlendMode::Function::One
                                                  : Renderer::BlendMode::Function::OneMinusSourceAlpha);

        shader->Bind();
        shader->SetUniformMat4x4("vertex_transformation", PresetState::orthogonalProjection);

        m_mesh.Indices().Resize((segments + 1) * 2);
        m_mesh.Indices().MakeContinuous();
        m_mesh.Update();
        m_mesh.Draw();
    }
}

} // namespace MilkdropPreset
} // namespace libprojectM
