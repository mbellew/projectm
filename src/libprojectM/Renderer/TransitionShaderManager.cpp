#include "Renderer/TransitionShaderManager.hpp"

#include "Renderer/FileScanner.hpp"

#include "BuiltInTransitionsResources.hpp"

#include <Logging.hpp>
#include <Utils.hpp>

#include <fstream>
#include <functional>
#include <sstream>

namespace libprojectM {
namespace Renderer {

TransitionShaderManager::TransitionShaderManager()
    : m_builtInShaders({CompileTransitionShader(kTransitionShaderBuiltInCircleGlsl330),
                        CompileTransitionShader(kTransitionShaderBuiltInPlasmaGlsl330),
                        CompileTransitionShader(kTransitionShaderBuiltInSimpleBlendGlsl330),
                        CompileTransitionShader(kTransitionShaderBuiltInSweepGlsl330),
                        CompileTransitionShader(kTransitionShaderBuiltInWarpGlsl330),
                        CompileTransitionShader(kTransitionShaderBuiltInZoomBlurGlsl330)})
    , m_mersenneTwister(m_randomDevice())
{
}

void TransitionShaderManager::LoadFromPaths(const std::vector<std::string>& paths)
{
    m_loadedShaders.clear();
    m_shadersByName.clear();

    if (paths.empty())
    {
        return;
    }

    std::vector<std::string> extensions{".frag", ".glsl"};
    FileScanner fileScanner(paths, extensions);

    using namespace std::placeholders;
    fileScanner.Scan(std::bind(&TransitionShaderManager::AddTransitionFile, this, _1, _2));

    LOG_INFO("[TransitionShaderManager] Loaded " + std::to_string(m_loadedShaders.size()) +
             " transition shader(s) from the transitions path.");
}

void TransitionShaderManager::AddTransitionFile(const std::string& path, const std::string& basename)
{
    std::ifstream file(path);
    if (!file)
    {
        LOG_WARN("[TransitionShaderManager] Could not open transition shader: " + path);
        return;
    }

    std::stringstream contents;
    contents << file.rdbuf();

    auto shader = CompileTransitionShader(contents.str());
    if (!shader)
    {
        // Skip it and keep scanning: one broken shader must not cost the user all the others.
        LOG_WARN("[TransitionShaderManager] Skipping transition shader that failed to compile: " + path);
        return;
    }

    m_shadersByName[Utils::ToLower(basename)] = shader;
    m_loadedShaders.push_back(std::move(shader));
}

auto TransitionShaderManager::RandomTransition() -> std::shared_ptr<Shader>
{
    // Prefer what was loaded from disk: the built-ins ship as files too, so the on-disk set is a
    // superset (built-ins plus whatever the user added). Fall back to the compiled-in copies when
    // the transitions path is missing, empty, or every file in it failed to compile.
    const auto& pool = m_loadedShaders.empty() ? m_builtInShaders : m_loadedShaders;

    if (pool.empty())
    {
        return {};
    }

    return pool.at(m_mersenneTwister() % pool.size());
}

auto TransitionShaderManager::TransitionByName(const std::string& name) -> std::shared_ptr<Shader>
{
    const auto shader = m_shadersByName.find(Utils::ToLower(name));
    if (shader == m_shadersByName.end())
    {
        return {};
    }

    return shader->second;
}

auto TransitionShaderManager::CompileTransitionShader(const std::string& shaderBodyCode) -> std::shared_ptr<Shader>
{
#ifdef USE_GLES
    // GLES also requires a precision specifier for variables and 3D samplers
    constexpr char versionHeader[] = "#version 300 es\n\nprecision mediump float;\nprecision mediump sampler3D;\n";
#else
    constexpr char versionHeader[] = "#version 330\n\n";
#endif

    std::string fragmentShaderSource(static_cast<const char*>(versionHeader));
    fragmentShaderSource.append(kTransitionShaderHeaderGlsl330);
    fragmentShaderSource.append("\n");
    fragmentShaderSource.append(shaderBodyCode);
    fragmentShaderSource.append("\n");
    fragmentShaderSource.append(kTransitionShaderMainGlsl330);

    try
    {
        auto transitionShader = std::make_shared<Shader>();
        transitionShader->CompileProgram(static_cast<const char*>(versionHeader) + kTransitionVertexShaderGlsl330, fragmentShaderSource);
        return transitionShader;
    }
    catch (const ShaderException& ex)
    {
        LOG_ERROR("[TransitionShaderManager] Transition shader failed to compile: " + ex.message());
        return {};
    }
}

} // namespace Renderer
} // namespace libprojectM
