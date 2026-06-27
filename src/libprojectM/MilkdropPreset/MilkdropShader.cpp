#include "MilkdropShader.hpp"

#include "PresetState.hpp"
#include "Utils.hpp"

#include <MilkdropStaticShaders.hpp>

#include <Engine.h> // M4::Log_ClearError / Log_GetLastError (captured hlslparser diagnostics)
#include <GLSLGenerator.h>
#include <HLSLParser.h>
#include <Logging.hpp>

#include <glm/gtc/matrix_transform.hpp>
#include <glm/mat4x4.hpp>

#include <algorithm>
#include <iomanip>
#include <regex>
#include <set>
#include <sstream>

namespace libprojectM {
namespace MilkdropPreset {

using libprojectM::MilkdropPreset::MilkdropStaticShaders;

namespace {
// Prefix each source line with its number ("  12 | ..."), so a transpiler/driver "line N"
// diagnostic maps directly onto the dumped source.
auto NumberShaderLines(const std::string& source) -> std::string
{
    std::istringstream input(source);
    std::ostringstream output;
    std::string line;
    int lineNumber = 1;
    while (std::getline(input, line))
    {
        output << std::setw(4) << lineNumber++ << " | " << line << '\n';
    }
    return output.str();
}

// Throw a verbose ShaderException for a failed HLSL->GLSL transpile stage. hlslparser reports the
// precise cause (offending token, line, expected type) via Log_Error, which we capture; include it
// in the summary, and log the line-numbered source that failed at ERROR so the offending line is
// visible without enabling debug logging. The concise summary is what the catch sites re-log.
[[noreturn]] void ThrowTranspileError(const std::string& shaderType, const std::string& stage,
                                      const std::string& failedSource)
{
    std::string summary = "[MilkdropShader] " + shaderType + " shader: " + stage + " failed";
    const std::string parserDiagnostic = M4::Log_GetLastError();
    if (!parserDiagnostic.empty())
    {
        summary += ": " + parserDiagnostic;
    }
    // Trim trailing newlines the parser may append, keeping the summary a single clean line.
    while (!summary.empty() && (summary.back() == '\n' || summary.back() == '\r'))
    {
        summary.pop_back();
    }

    LOG_ERROR(summary + "\n--- failed " + shaderType + " shader source (line-numbered) ---\n" +
              NumberShaderLines(failedSource));
    throw Renderer::ShaderException(summary);
}
} // namespace

static auto floatRand = []() { return static_cast<float>(rand() % 7381) / 7380.0f; };

MilkdropShader::MilkdropShader(ShaderType type)
    : m_type(type)
    , m_randValues({floatRand(), floatRand(), floatRand(), floatRand()})
{
    unsigned int index = 0;
    do
    {
        for (int i = 0; i < 4; i++)
        {
            float const m_randTranslationMult = 1;
            float const rotMult = 0.9f * powf(index / 8.0f, 3.2f);
            m_randTranslation[index].x = (floatRand() * 2 - 1) * m_randTranslationMult;
            m_randTranslation[index].y = (floatRand() * 2 - 1) * m_randTranslationMult;
            m_randTranslation[index].z = (floatRand() * 2 - 1) * m_randTranslationMult;
            m_randRotationCenters[index].x = floatRand() * 6.28f;
            m_randRotationCenters[index].y = floatRand() * 6.28f;
            m_randRotationCenters[index].z = floatRand() * 6.28f;
            m_randRotationSpeeds[index].x = (floatRand() * 2 - 1) * rotMult;
            m_randRotationSpeeds[index].y = (floatRand() * 2 - 1) * rotMult;
            m_randRotationSpeeds[index].z = (floatRand() * 2 - 1) * rotMult;
            index++;
        }
    } while (index < sizeof(m_randTranslation) / sizeof(m_randTranslation[0]));
}

void MilkdropShader::LoadCode(const std::string& presetShaderCode)
{
    m_fragmentShaderCode = presetShaderCode;
    m_preprocessedCode = m_fragmentShaderCode;

    GetReferencedSamplers(m_preprocessedCode);
    PreprocessPresetShader(m_preprocessedCode);
}

void MilkdropShader::LoadTexturesAndCompile(PresetState& presetState)
{
    std::locale loc;

    // Now request the textures and descriptors from the texture manager.
    for (const auto& name : m_samplerNames)
    {
        std::string baseName = name;
        if (name.length() > 3 && name.at(2) == '_')
        {
            baseName = name.substr(3);
        }

        std::string lowerCaseName = Utils::ToLower(baseName);

        // The "main" and "blurX" textures are preset-specific and are not managed by TextureManager.
        if (lowerCaseName == "main")
        {
            Renderer::TextureSamplerDescriptor desc(presetState.mainTexture.lock(),
                                                    presetState.renderContext.textureManager->GetSampler(name),
                                                    name,
                                                    "main");
            m_mainTextureDescriptors.push_back(std::move(desc));
            continue;
        }

        // The color palette is preset-specific (PALETTE_NAME), baked once into per-preset 2D LUTs
        // (sRGB for Palette()/PaletteMap, OKLab for snap/pull). Bound here like "main" rather than
        // via TextureManager. Resolved+fixed at load; the bake happens lazily on this (GL) thread.
        if (lowerCaseName == "palette" || lowerCaseName == "palette_lab")
        {
            const auto& texture = (lowerCaseName == "palette") ? presetState.palette.SrgbTexture()
                                                               : presetState.palette.LabTexture();
            Renderer::TextureSamplerDescriptor desc(texture,
                                                    presetState.renderContext.textureManager->GetSampler(name),
                                                    name,
                                                    baseName);
            m_textureSamplerDescriptors.push_back(std::move(desc));
            continue;
        }

        // A few presets directly use the (undocumented) sampler name.
        if (lowerCaseName == "blur1")
        {
            UpdateMaxBlurLevel(BlurTexture::BlurLevel::Blur1);
            continue;
        }
        if (lowerCaseName == "blur2")
        {
            UpdateMaxBlurLevel(BlurTexture::BlurLevel::Blur2);
            continue;
        }
        if (lowerCaseName == "blur3")
        {
            UpdateMaxBlurLevel(BlurTexture::BlurLevel::Blur3);
            continue;
        }

        // Random textures need special treatment.
        if (lowerCaseName.length() >= 6 &&
            lowerCaseName.substr(0, 4) == "rand" && std::isdigit(lowerCaseName.at(4), loc) && std::isdigit(lowerCaseName.at(5), loc))
        {
            // First look up the random texture index in the preset state so the texture matches between warp and composite shaders
            int randomSlot = -1;
            try
            {
                randomSlot = std::stoi(lowerCaseName.substr(4, 2));
            }
            catch (...) // Ignore any conversion errors.
            {
            }

            if (randomSlot >= 0 && randomSlot <= 15)
            {
                if (presetState.randomTextureDescriptors.find(randomSlot) != presetState.randomTextureDescriptors.end())
                {
                    // Use existing texture descriptor.
                    m_textureSamplerDescriptors.push_back(presetState.randomTextureDescriptors.at(randomSlot));
                    continue;
                }

                // Slot empty, request a new random texture.
                auto desc = presetState.renderContext.textureManager->GetRandomTexture(name);

                // Also store a copy in preset state!
                presetState.randomTextureDescriptors.insert({randomSlot, desc});

                m_textureSamplerDescriptors.push_back(std::move(desc));
                continue;
            }

            // Fall through if slot number is out of range and treat as normal texture.
        }

        auto desc = presetState.renderContext.textureManager->GetTexture(name);
        m_textureSamplerDescriptors.push_back(std::move(desc));
    }

    // Now that we have the textures, transpile the code.
    TranspileHLSLShader(presetState, m_preprocessedCode);

    // Update blur texture level if shader was compiled successfully.
    presetState.blurTexture.SetRequiredBlurLevel(m_maxBlurLevelRequired);
}

void MilkdropShader::LoadVariables(const PresetState& presetState, const PerFrameContext& perFrameContext)
{
    // These are the inputs: http://www.geisswerks.com/milkdrop/milkdrop_preset_authoring.html#3f6

    auto floatTime = static_cast<float>(presetState.renderContext.time);
    auto timeSincePresetStartWrapped = floatTime - static_cast<int>(floatTime / 10000.0) * 10000;
    auto mipX = logf(static_cast<float>(presetState.renderContext.viewportSizeX)) / logf(2.0f);
    auto mipY = logf(static_cast<float>(presetState.renderContext.viewportSizeY)) / logf(2.0f);
    auto mipAvg = 0.5f * (mipX + mipY);

    BlurTexture::Values blurMin;
    BlurTexture::Values blurMax;
    BlurTexture::GetSafeBlurMinMaxValues(perFrameContext, blurMin, blurMax);

    m_shader.Bind();

    m_shader.SetUniformMat4x4("vertex_transformation", PresetState::orthogonalProjection);

    m_shader.SetUniformFloat4("rand_frame", {floatRand(),
                                             floatRand(),
                                             floatRand(),
                                             floatRand()});
    m_shader.SetUniformFloat4("rand_preset", {m_randValues[0],
                                              m_randValues[1],
                                              m_randValues[2],
                                              m_randValues[3]});

    m_shader.SetUniformFloat4("_c0", {presetState.renderContext.aspectX,
                                      presetState.renderContext.aspectY,
                                      1.0f / presetState.renderContext.aspectX,
                                      1.0f / presetState.renderContext.aspectY});
    m_shader.SetUniformFloat4("_c1", {0.0,
                                      0.0,
                                      0.0,
                                      0.0});
    m_shader.SetUniformFloat4("_c2", {timeSincePresetStartWrapped,
                                      presetState.renderContext.fps,
                                      presetState.renderContext.frame,
                                      presetState.renderContext.progress});
    m_shader.SetUniformFloat4("_c3", {presetState.audioData.bass,
                                      presetState.audioData.mid,
                                      presetState.audioData.treb,
                                      presetState.audioData.vol});
    m_shader.SetUniformFloat4("_c4", {presetState.audioData.bassAtt,
                                      presetState.audioData.midAtt,
                                      presetState.audioData.trebAtt,
                                      presetState.audioData.volAtt});
    m_shader.SetUniformFloat4("_c5", {blurMax[0] - blurMin[0],
                                      blurMin[0],
                                      blurMax[1] - blurMin[1],
                                      blurMin[1]});
    m_shader.SetUniformFloat4("_c6", {blurMax[2] - blurMin[2],
                                      blurMin[2],
                                      blurMin[0],
                                      blurMax[0]});
    m_shader.SetUniformFloat4("_c7", {presetState.renderContext.viewportSizeX,
                                      presetState.renderContext.viewportSizeY,
                                      1.0f / static_cast<float>(presetState.renderContext.viewportSizeX),
                                      1.0f / static_cast<float>(presetState.renderContext.viewportSizeY)});

    m_shader.SetUniformFloat4("_c8", {0.5f + 0.5f * cosf(floatTime * 0.329f + 1.2f),
                                      0.5f + 0.5f * cosf(floatTime * 1.293f + 3.9f),
                                      0.5f + 0.5f * cosf(floatTime * 5.070f + 2.5f),
                                      0.5f + 0.5f * cosf(floatTime * 20.051f + 5.4f)});

    m_shader.SetUniformFloat4("_c9", {0.5f + 0.5f * sinf(floatTime * 0.329f + 1.2f),
                                      0.5f + 0.5f * sinf(floatTime * 1.293f + 3.9f),
                                      0.5f + 0.5f * sinf(floatTime * 5.070f + 2.5f),
                                      0.5f + 0.5f * sinf(floatTime * 20.051f + 5.4f)});

    m_shader.SetUniformFloat4("_c10", {0.5f + 0.5f * cosf(floatTime * 0.0050f + 2.7f),
                                       0.5f + 0.5f * cosf(floatTime * 0.0085f + 5.3f),
                                       0.5f + 0.5f * cosf(floatTime * 0.0133f + 4.5f),
                                       0.5f + 0.5f * cosf(floatTime * 0.0217f + 3.8f)});

    m_shader.SetUniformFloat4("_c11", {0.5f + 0.5f * sinf(floatTime * 0.0050f + 2.7f),
                                       0.5f + 0.5f * sinf(floatTime * 0.0085f + 5.3f),
                                       0.5f + 0.5f * sinf(floatTime * 0.0133f + 4.5f),
                                       0.5f + 0.5f * sinf(floatTime * 0.0217f + 3.8f)});

    m_shader.SetUniformFloat4("_c12", {mipX,
                                       mipY,
                                       mipAvg,
                                       0});
    m_shader.SetUniformFloat4("_c13", {blurMin[1],
                                       blurMax[1],
                                       blurMin[2],
                                       blurMax[2]});

    m_shader.SetUniformFloat("video_z_write", presetState.renderContext.videoZWrite);
    m_shader.SetUniformFloat("video_z_range", presetState.renderContext.videoZRange);
    m_shader.SetUniformFloat("video_frame_count", presetState.renderContext.videoFrameCount);
    m_shader.SetUniformFloat("video_buffer_seconds", presetState.renderContext.videoBufferSeconds);

    m_shader.SetUniformFloat("beat_phase", presetState.audioData.beatPhase);
    m_shader.SetUniformFloat("beat_onset", presetState.audioData.beatOnset);
    m_shader.SetUniformFloat("beat_bpm", presetState.audioData.bpm);
    m_shader.SetUniformFloat("beat_conf", presetState.audioData.beatConf);

    m_shader.SetUniformFloat("seg_cx", presetState.renderContext.segCx);
    m_shader.SetUniformFloat("seg_cy", presetState.renderContext.segCy);
    m_shader.SetUniformFloat("seg_vx", presetState.renderContext.segVx);
    m_shader.SetUniformFloat("seg_vy", presetState.renderContext.segVy);
    m_shader.SetUniformFloat("seg_coverage", presetState.renderContext.segCoverage);
    m_shader.SetUniformFloat("seg_valid", presetState.renderContext.segValid);


    std::array<glm::mat4, 24> tempMatrices{};

    // write matrices
    for (int i = 0; i < 20; i++)
    {
        glm::mat4 const rotationX = glm::rotate(glm::mat4(1.0f), m_randRotationCenters[i].x + m_randRotationSpeeds[i].x * floatTime, glm::vec3(1.0f, 0.0f, 0.0f));
        glm::mat4 const rotationY = glm::rotate(glm::mat4(1.0f), m_randRotationCenters[i].y + m_randRotationSpeeds[i].y * floatTime, glm::vec3(0.0f, 1.0f, 0.0f));
        glm::mat4 const rotationZ = glm::rotate(glm::mat4(1.0f), m_randRotationCenters[i].z + m_randRotationSpeeds[i].z * floatTime, glm::vec3(0.0f, 0.0f, 1.0f));

        glm::mat4 const randomTranslation = glm::translate(glm::mat4(1.0f), glm::vec3(m_randTranslation[i].x, m_randTranslation[i].y, m_randTranslation[i].z));

        tempMatrices[i] = randomTranslation * rotationX;
        tempMatrices[i] = rotationZ * tempMatrices[i];
        tempMatrices[i] = rotationY * tempMatrices[i];
    }

    // the last 4 are totally random, each frame
    for (int i = 20; i < 24; i++)
    {
        glm::mat4 const rotationX = glm::rotate(glm::mat4(1.0f), floatRand() * 6.28f, glm::vec3(1.0f, 0.0f, 0.0f));
        glm::mat4 const rotationY = glm::rotate(glm::mat4(1.0f), floatRand() * 6.28f, glm::vec3(0.0f, 1.0f, 0.0f));
        glm::mat4 const rotationZ = glm::rotate(glm::mat4(1.0f), floatRand() * 6.28f, glm::vec3(0.0f, 0.0f, 1.0f));

        glm::mat4 const randomTranslation = glm::translate(glm::mat4(1.0f), glm::vec3(floatRand(), floatRand(), floatRand()));

        tempMatrices[i] = randomTranslation * rotationX;
        tempMatrices[i] = rotationZ * tempMatrices[i];
        tempMatrices[i] = rotationY * tempMatrices[i];
    }

    m_shader.SetUniformMat3x4("rot_s1", tempMatrices[0]);
    m_shader.SetUniformMat3x4("rot_s2", tempMatrices[1]);
    m_shader.SetUniformMat3x4("rot_s3", tempMatrices[2]);
    m_shader.SetUniformMat3x4("rot_s4", tempMatrices[3]);
    m_shader.SetUniformMat3x4("rot_d1", tempMatrices[4]);
    m_shader.SetUniformMat3x4("rot_d2", tempMatrices[5]);
    m_shader.SetUniformMat3x4("rot_d3", tempMatrices[6]);
    m_shader.SetUniformMat3x4("rot_d4", tempMatrices[7]);
    m_shader.SetUniformMat3x4("rot_f1", tempMatrices[8]);
    m_shader.SetUniformMat3x4("rot_f2", tempMatrices[9]);
    m_shader.SetUniformMat3x4("rot_f3", tempMatrices[10]);
    m_shader.SetUniformMat3x4("rot_f4", tempMatrices[11]);
    m_shader.SetUniformMat3x4("rot_vf1", tempMatrices[12]);
    m_shader.SetUniformMat3x4("rot_vf2", tempMatrices[13]);
    m_shader.SetUniformMat3x4("rot_vf3", tempMatrices[14]);
    m_shader.SetUniformMat3x4("rot_vf4", tempMatrices[15]);
    m_shader.SetUniformMat3x4("rot_uf1", tempMatrices[16]);
    m_shader.SetUniformMat3x4("rot_uf2", tempMatrices[17]);
    m_shader.SetUniformMat3x4("rot_uf3", tempMatrices[18]);
    m_shader.SetUniformMat3x4("rot_uf4", tempMatrices[19]);
    m_shader.SetUniformMat3x4("rot_rand1", tempMatrices[20]);
    m_shader.SetUniformMat3x4("rot_rand2", tempMatrices[21]);
    m_shader.SetUniformMat3x4("rot_rand3", tempMatrices[22]);
    m_shader.SetUniformMat3x4("rot_rand4", tempMatrices[23]);

    // set program uniform "_q[a-h]" values (_qa.x, _qa.y, _qa.z, _qa.w, _qb.x, _qb.y ... ) alias q[1-32]
    for (int i = 0; i < QVarCount; i += 4)
    {
        std::string varName = "_q";
        varName.push_back(static_cast<char>('a' + i / 4));
        m_shader.SetUniformFloat4(varName.c_str(), {presetState.frameQVariables[i],
                                                    presetState.frameQVariables[i + 1],
                                                    presetState.frameQVariables[i + 2],
                                                    presetState.frameQVariables[i + 3]});
    }

    // Bind all texture and sampler descriptors. This includes the main and blur textures.
    GLint textureUnit{0};
    for (auto& desc : m_mainTextureDescriptors)
    {
        // Update main texture, swaps every frame.
        desc.Texture(presetState.mainTexture);
        desc.Bind(textureUnit, m_shader);
        textureUnit++;
    }
    presetState.blurTexture.Bind(textureUnit, m_shader);
    for (auto& desc : m_textureSamplerDescriptors)
    {
        if (desc.Empty())
        {
            desc.TryUpdate(*presetState.renderContext.textureManager);
        }
        desc.Bind(textureUnit, m_shader);
        textureUnit++;
    }
}

auto MilkdropShader::Shader() -> Renderer::Shader&
{
    return m_shader;
}

void MilkdropShader::PreprocessPresetShader(std::string& program)
{
    std::string shaderTypeString = "composite";
    if (m_type == ShaderType::WarpShader)
    {
        shaderTypeString = "warp";
    }
    else if (m_type == ShaderType::VideoShader)
    {
        shaderTypeString = "video";
    }

    if (program.length() <= 0)
    {
        throw Renderer::ShaderException("[MilkdropShader] Preset " + shaderTypeString + " shader is declared, but empty.");
    }

    size_t found;

    // Find "sampler_state" overrides and remove them first, as they're not supported by GLSL.
    // The logic isn't totally fool-proof, but should work in general.
    // Use a comment-stripped copy for searching so commented-out sampler_state blocks are skipped.
    // StripComments preserves string length, so positions map 1:1 to the original.
    std::string stripped = Utils::StripComments(program);
    found = stripped.find("sampler_state");
    while (found != std::string::npos)
    {
        // Now go backwards and find the assignment
        found = stripped.rfind('=', found);
        auto startPos = found;

        // Find closing brace and semicolon
        found = stripped.find('}', found);
        found = stripped.find(';', found);

        if (found != std::string::npos)
        {
            stripped.replace(startPos, found - startPos, "");
        }
        else
        {
            // No closing brace and semicolon.
            break;
        }

        found = stripped.find("sampler_state");
    }

    // replace shader_body with entry point function
    // Use the stripped copy so a commented-out shader_body is not matched.
    found = stripped.find("shader_body");
    if (found != std::string::npos)
    {
        if (m_type == ShaderType::WarpShader)
        {
            program.replace(int(found), 11, R"(
void PS(float4 _vDiffuse : COLOR,
        float4 _uv : TEXCOORD0,
        float2 _rad_ang : TEXCOORD1,
        out float4 _return_value : COLOR0,
        out float4 _mv_tex_coords : COLOR1)
)");
        }
        else if (m_type == ShaderType::VideoShader)
        {
            // Fullscreen pass: only the texture coordinate is interpolated (no per-pixel mesh,
            // so no rad/ang, no hue diffuse). Single output -> the processed video frame.
            program.replace(int(found), 11, R"(
void PS(float2 _uv : TEXCOORD0,
        out float4 _return_value : COLOR)
)");
        }
        else
        {
            program.replace(int(found), 11, R"(
void PS(float4 _vDiffuse : COLOR,
        float2 _uv : TEXCOORD0,
        float2 _rad_ang : TEXCOORD1,
        out float4 _return_value : COLOR)
)");
        }
    }
    else
    {
        LOG_DEBUG("[MilkdropShader] Failed " + shaderTypeString + " shader code:\n" + program);
        throw Renderer::ShaderException("[MilkdropShader] Preset " + shaderTypeString + " shader is missing \"shader_body\" entry point.");
    }

    // replace the "{" immediately following shader_body with some variable declarations
    found = program.find('{', found);
    if (found != std::string::npos)
    {
        std::string progMain = "{\nfloat3 ret = 0;\n";
        if (m_type == ShaderType::WarpShader)
        {
            progMain.append("_mv_tex_coords.xy = _uv.xy;\n");
            /*FLOATBUF*/ // Expose the alpha channel as a writable per-pixel state ("ret_a"). Default
            /*FLOATBUF*/ // to the warped feedback alpha so untouched presets carry state with the pixels.
            progMain.append("float ret_a = tex2D(sampler_main, _uv.xy).a;\n");
        }
        else if (m_type == ShaderType::VideoShader)
        {
            // Default to passing the live frame through unchanged: rgb from the incoming frame and
            // ret_a = the alpha the preset's video_alpha_mode already computed (carried in the
            // input's alpha). A video_ shader overrides ret_a (and optionally ret) to author what
            // goes into the history.
            progMain = "{\nfloat3 ret = GetVideoIn(uv);\n";
            progMain.append("float ret_a = tex2D(sampler_video_in, uv).a;\n");
        }
        else
        {
            /*FLOATBUF*/ // Comp output is display-only (not fed back): default opaque, override via ret_a.
            progMain.append("float ret_a = 1.0;\n");
        }
        program.replace(int(found), 1, progMain);
    }
    else
    {
        LOG_DEBUG("[MilkdropShader] Failed " + shaderTypeString + " shader code:\n" + program);
        throw Renderer::ShaderException("[MilkdropShader] Preset " + shaderTypeString + " shader has no opening braces.");
    }

    // replace "}" with return statement (this can probably be optimized for the GLSL conversion...)
    found = program.rfind('}');
    if (found != std::string::npos)
    {
        program.replace(int(found), 1, "_return_value = float4(ret.xyz, ret_a);\n" /*FLOATBUF*/
                                       "}\n");
    }
    else
    {
        LOG_DEBUG("[MilkdropShader] Failed " + shaderTypeString + " shader code:\n" + program);
        throw Renderer::ShaderException("[MilkdropShader] Preset " + shaderTypeString + " shader has no closing brace.");
    }

    // Find matching closing brace and cut off excess text after shader's main function
    int bracesOpen = 1;
    size_t pos = found + 1;
    for (; pos < program.length() && bracesOpen > 0; ++pos)
    {
        switch (program.at(pos))
        {
            case '/':
                // Skip line comments until EoL to prevent false counting
                if (pos < program.length() - 1 && program.at(pos + 1) == '/')
                {
                    for (; pos < program.length(); ++pos)
                    {
                        if (program.at(pos) == '\n')
                        {
                            break;
                        }
                    }
                }
                // Skip block comments to prevent false counting
                else if (pos < program.length() - 1 && program.at(pos + 1) == '*')
                {
                    pos += 2;
                    for (; pos < program.length() - 1; ++pos)
                    {
                        if (program.at(pos) == '*' && program.at(pos + 1) == '/')
                        {
                            ++pos; // skip past '/'
                            break;
                        }
                    }
                }
                continue;

            case '{':
                bracesOpen++;
                continue;

            case '}':
                bracesOpen--;
        }
    }

    if (pos < program.length() - 1)
    {
        program.resize(pos);
    }

    std::string fullSource; //!< Full shader source before translation, includes all uniforms etc.

    // First copy the generic "header" into the shader. Includes uniforms and some defines
    // to unwrap the packed 4-element uniforms into single values.
    fullSource.append(MilkdropStaticShaders::Get()->GetPresetShaderHeader());

    if (m_type == ShaderType::WarpShader)
    {
        fullSource.append("#define rad _rad_ang.x\n"
                          "#define ang _rad_ang.y\n"
                          "#define uv _uv.xy\n"
                          "#define uv_orig _uv.zw\n");
    }
    else if (m_type == ShaderType::VideoShader)
    {
        // No per-pixel mesh: only uv is meaningful (rad/ang/hue come from the warp/comp mesh).
        fullSource.append("#define uv _uv\n"
                          "#define uv_orig _uv\n");
    }
    else
    {
        fullSource.append("#define rad _rad_ang.x\n"
                          "#define ang _rad_ang.y\n"
                          "#define uv _uv.xy\n"
                          "#define uv_orig _uv.xy\n"
                          "#define hue_shader _vDiffuse.xyz\n");
    }

    fullSource.append(program);

    program = fullSource;
}

void MilkdropShader::GetReferencedSamplers(const std::string& program)
{
    // Look up samplers referenced in the shader program
    m_samplerNames.clear();

    // "main" should always be present.
    m_samplerNames.insert("main");

    // Strip comments so that commented-out sampler/texsize declarations are not matched.
    std::string const stripped = Utils::StripComments(program);

    // Search for sampler usage
    auto found = stripped.find("sampler_", 0);
    while (found != std::string::npos)
    {
        found += 8;
        size_t const end = stripped.find_first_of(" ;,\n\r)", found);

        if (end != std::string::npos)
        {
            std::string const sampler = stripped.substr(static_cast<int>(found), static_cast<int>(end - found));
            // Skip "sampler_state", as it's a reserved word and not a sampler.
            if (sampler != "state")
            {
                m_samplerNames.insert(sampler);
            }
        }

        found = stripped.find("sampler_", found);
    }

    // Also search for texsize usage, some presets don't reference the sampler.
    found = stripped.find("texsize_", 0);
    while (found != std::string::npos)
    {
        found += 8;
        size_t const end = stripped.find_first_of(" ;,.\n\r)", found);

        if (end != std::string::npos)
        {
            std::string const sampler = stripped.substr(static_cast<int>(found), static_cast<int>(end - found));
            m_samplerNames.insert(sampler);
        }

        found = stripped.find("texsize_", found);
    }

    {
        // Remove duplicate mentions or "randXX" names, keeping the long forms only (first one will determine the actual texture loaded).
        auto samplerName = m_samplerNames.begin();
        std::locale loc;
        while (samplerName != m_samplerNames.end())
        {
            std::string lowerCaseName = Utils::ToLower(*samplerName);
            if (lowerCaseName.length() == 6 &&
                lowerCaseName.substr(0, 4) == "rand" && std::isdigit(lowerCaseName.at(4), loc) && std::isdigit(lowerCaseName.at(5), loc))
            {
                auto additionalName = samplerName;
                additionalName++;
                if (additionalName != m_samplerNames.end())
                {
                    std::string addLowerCaseName = Utils::ToLower(*additionalName);
                    if (addLowerCaseName.length() > 7 &&
                        addLowerCaseName.substr(0, 6) == lowerCaseName &&
                        addLowerCaseName[6] == '_')
                    {
                        samplerName = m_samplerNames.erase(samplerName);
                    }
                }
            }
            samplerName++;
        }
    }

    // GetBlur#() reads the blurred color, GetBlurA#() the blurred alpha. Both expand to
    // tex2D(sampler_blurN, ...) during preprocessing (after this scan), so either form must
    // trigger declaration of the matching blur sampler. "GetBlur3" is a substring of neither
    // "GetBlurA3" nor the lower levels, so the alpha variants must be matched explicitly.
    if (stripped.find("GetBlur3") != std::string::npos || stripped.find("GetBlurA3") != std::string::npos)
    {
        UpdateMaxBlurLevel(BlurTexture::BlurLevel::Blur3);
    }
    else if (stripped.find("GetBlur2") != std::string::npos || stripped.find("GetBlurA2") != std::string::npos)
    {
        UpdateMaxBlurLevel(BlurTexture::BlurLevel::Blur2);
    }
    else if (stripped.find("GetBlur1") != std::string::npos || stripped.find("GetBlurA1") != std::string::npos)
    {
        UpdateMaxBlurLevel(BlurTexture::BlurLevel::Blur1);
    }
    else
    {
        m_maxBlurLevelRequired = BlurTexture::BlurLevel::None;
    }

    // GetVideo(uv, age) is a macro that expands to tex3D(sampler_fw_video, ...) during
    // HLSL preprocessing, after this scan runs. Without this, the video texture would
    // never be bound for shaders that only reference it via the macro.
    if (stripped.find("GetVideo") != std::string::npos)
    {
        m_samplerNames.insert("fw_video");
    }

    // The mask buffer accessors (GetMask / MaskSeg / MaskSegBlur / MaskMotion / MaskMotionDecay)
    // all expand to tex2D(sampler_fc_mask, ...) during HLSL preprocessing, after this scan. Any
    // reference to "Mask" must therefore declare the mask sampler. See MASK_LAYERS.md.
    if (stripped.find("Mask") != std::string::npos)
    {
        m_samplerNames.insert("fc_mask");
    }

    // The palette LUT samplers are ALWAYS declared: the header defines PaletteSnap/PalettePull as
    // helper functions (a loop can't live in a #define), and those reference sampler_fc_palette
    // (sRGB LUT) and sampler_fc_palette_lab (OKLab LUT for perceptual distance/blend)
    // unconditionally, so both must always exist even when a preset uses no palette (the LUTs are
    // always registered; unused functions are dead-stripped by the GLSL compiler). See ColorPalette.
    m_samplerNames.insert("fc_palette");
    m_samplerNames.insert("fc_palette_lab");
}

void MilkdropShader::TranspileHLSLShader(const PresetState& presetState, std::string& program)
{
    std::string shaderTypeString = "composite";
    if (m_type == ShaderType::WarpShader)
    {
        shaderTypeString = "warp";
    }

    M4::GLSLGenerator generator;
    M4::Allocator allocator;

    M4::HLSLTree tree(&allocator);
    M4::HLSLParser parser(&allocator, &tree);

    // Preprocess define macros
    std::string sourcePreprocessed;
    M4::Log_ClearError();
    if (!parser.ApplyPreprocessor("", program.c_str(), program.size(), sourcePreprocessed))
    {
        ThrowTranspileError(shaderTypeString, "preprocessing", program);
    }

    // Remove previous shader declarations
    // ToDo: Quite some presets declare a sampler_state{} struct to change the wrap mode.
    //       The below code causes invalid syntax as it leaves part of the expression.
    //       Leaving it in causes HLSLParser to add "sampler_XYZ = sampler2D( <unknown expression> );"
    //       in the main() function, which is also bad...
    std::smatch matches;
    while (std::regex_search(sourcePreprocessed, matches, std::regex("sampler(2D|3D|)(\\s+|\\().*")))
    {
        sourcePreprocessed.replace(matches.position(), matches.length(), "");
    }

    // Remove previous texsize declarations
    while (std::regex_search(sourcePreprocessed, matches, std::regex("float4\\s+texsize_.*")))
    {
        sourcePreprocessed.replace(matches.position(), matches.length(), "");
    }

    // Collect unique samplers and texsize uniforms
    std::set<std::string> samplerDeclarations;
    std::set<std::string> texSizeDeclarations;
    for (const auto& desc : m_mainTextureDescriptors)
    {
        samplerDeclarations.insert(desc.SamplerDeclaration());
        texSizeDeclarations.insert(desc.TexSizeDeclaration());
    }
    for (const auto& desc : presetState.blurTexture.GetDescriptorsForBlurLevel(m_maxBlurLevelRequired))
    {
        samplerDeclarations.insert(desc.SamplerDeclaration());
        // No texsize_blur1 etc.
    }
    for (const auto& desc : m_textureSamplerDescriptors)
    {
        samplerDeclarations.insert(desc.SamplerDeclaration());
        texSizeDeclarations.insert(desc.TexSizeDeclaration());
    }

    // Now insert them on top.
    for (const auto& texSizeDeclaration : texSizeDeclarations)
    {
        sourcePreprocessed.insert(0, texSizeDeclaration);
    }
    for (const auto& samplerDeclaration : samplerDeclarations)
    {
        sourcePreprocessed.insert(0, samplerDeclaration);
    }

    // Transpile from HLSL (aka preset shader aka DirectX shader) to GLSL (aka OpenGL shader lang)
    // First, parse HLSL into a tree
    M4::Log_ClearError();
    if (!parser.Parse("", sourcePreprocessed.c_str(), sourcePreprocessed.size()))
    {
        ThrowTranspileError(shaderTypeString, "HLSL parse", sourcePreprocessed);
    }

    // Then generate GLSL from the resulting parser tree
    M4::Log_ClearError();
    if (!generator.Generate(&tree, M4::GLSLGenerator::Target_FragmentShader,
                            MilkdropStaticShaders::Get()->GetGlslGeneratorVersion(),
                            "PS", M4::GLSLGenerator::Options(M4::GLSLGenerator::Flag_AlternateNanPropagation)))
    {
        ThrowTranspileError(shaderTypeString, "GLSL generation", sourcePreprocessed);
    }

    LOG_TRACE("[MilkdropShader] Transpiled GLSL " + shaderTypeString + " shader code:\n" + std::string(generator.GetResult()));

    // Now we have GLSL source for the preset shader program (hopefully it's valid!)
    // Compile the preset shader fragment shader with the standard vertex shader and cross our fingers.
    if (m_type == ShaderType::WarpShader)
    {
        m_shader.CompileProgram(MilkdropStaticShaders::Get()->GetPresetWarpVertexShader(), generator.GetResult());
    }
    else
    {
        m_shader.CompileProgram(MilkdropStaticShaders::Get()->GetPresetCompVertexShader(), generator.GetResult());
    }
}

void MilkdropShader::CompileVideoShader()
{
    TranspileVideoShader(m_preprocessedCode);
}

void MilkdropShader::TranspileVideoShader(std::string& program)
{
    M4::GLSLGenerator generator;
    M4::Allocator allocator;

    M4::HLSLTree tree(&allocator);
    M4::HLSLParser parser(&allocator, &tree);

    // Preprocess define macros
    std::string sourcePreprocessed;
    M4::Log_ClearError();
    if (!parser.ApplyPreprocessor("", program.c_str(), program.size(), sourcePreprocessed))
    {
        ThrowTranspileError("video", "preprocessing", program);
    }

    // Strip any sampler / texsize declarations the author left in the body; the preprocess pass
    // owns its textures and we declare them ourselves below.
    std::smatch matches;
    while (std::regex_search(sourcePreprocessed, matches, std::regex("sampler(2D|3D|)(\\s+|\\().*")))
    {
        sourcePreprocessed.replace(matches.position(), matches.length(), "");
    }
    while (std::regex_search(sourcePreprocessed, matches, std::regex("float4\\s+texsize_.*")))
    {
        sourcePreprocessed.replace(matches.position(), matches.length(), "");
    }

    // Fixed set of preprocess-owned samplers (no TextureManager descriptors): the live frame, the
    // mask buffer and the history. The palette LUTs are always declared because the shared header's
    // PaletteSnap/PalettePull helpers reference them unconditionally; unused samplers (and those
    // helpers) are dead-stripped by the GLSL compiler.
    sourcePreprocessed.insert(0,
        "uniform sampler2D sampler_video_in;\n"
        "uniform sampler2D sampler_fc_mask;\n"
        "uniform sampler3D sampler_fw_video;\n"
        "uniform sampler3D sampler_fc_palette;\n"
        "uniform sampler3D sampler_fc_palette_lab;\n");

    M4::Log_ClearError();
    if (!parser.Parse("", sourcePreprocessed.c_str(), sourcePreprocessed.size()))
    {
        ThrowTranspileError("video", "HLSL parse", sourcePreprocessed);
    }

    M4::Log_ClearError();
    if (!generator.Generate(&tree, M4::GLSLGenerator::Target_FragmentShader,
                            MilkdropStaticShaders::Get()->GetGlslGeneratorVersion(),
                            "PS", M4::GLSLGenerator::Options(M4::GLSLGenerator::Flag_AlternateNanPropagation)))
    {
        ThrowTranspileError("video", "GLSL generation", sourcePreprocessed);
    }

    LOG_TRACE("[MilkdropShader] Transpiled GLSL video shader code:\n" + std::string(generator.GetResult()));

    m_shader.CompileProgram(MilkdropStaticShaders::Get()->GetPresetVideoVertexShader(), generator.GetResult());
}

void MilkdropShader::UpdateMaxBlurLevel(BlurTexture::BlurLevel requestedLevel)
{
    if (m_maxBlurLevelRequired >= requestedLevel)
    {
        return;
    }

    m_maxBlurLevelRequired = requestedLevel;

    if (m_maxBlurLevelRequired == BlurTexture::BlurLevel::Blur3)
    {
        m_samplerNames.insert("blur1");
        m_samplerNames.insert("blur2");
        m_samplerNames.insert("blur3");
    }
    else if (m_maxBlurLevelRequired == BlurTexture::BlurLevel::Blur2)
    {
        m_samplerNames.insert("blur1");
        m_samplerNames.insert("blur2");
    }
    else
    {
        m_samplerNames.insert("blur1");
    }
}

} // namespace MilkdropPreset
} // namespace libprojectM
