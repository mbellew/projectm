#pragma once

#include "Renderer/Shader.hpp"

#include <map>
#include <random>
#include <string>
#include <vector>

namespace libprojectM {
namespace Renderer {

/**
 * @brief Manages all available transition shaders.
 *
 * Transitions come from two places:
 *
 * - the BUILT-INS, compiled into the library (circle, sweep, warp, zoom blur, plasma, blend). They
 *   are the fallback, so a missing or empty transitions directory can never leave the engine with
 *   nothing to transition with.
 * - a TRANSITIONS DIRECTORY, scanned at runtime (LoadFromPaths). Shaders found there are keyed by
 *   filename -- which is what will let a preset ask for one by name (transition_in/transition_out).
 *
 * If the directory yields any shaders, the random pool is drawn from it instead of the built-ins:
 * the built-ins also ship as files, so they remain in the pool -- just editable rather than baked in.
 */
class TransitionShaderManager
{
public:
    TransitionShaderManager();

    /**
     * @brief Scans the given paths for transition shaders and compiles them.
     *
     * Each file holds a fragment shader body (a `mainImage()`), the same format as the built-ins:
     * iChannel0 is the outgoing preset's output, iChannel1 the incoming preset's. Requires a current
     * GL context. Replaces any previously loaded set. A file that fails to compile is skipped with a
     * warning rather than aborting the scan -- one bad shader must not cost you the others.
     *
     * @param paths Directories to scan (recursively) for transition shaders.
     */
    void LoadFromPaths(const std::vector<std::string>& paths);

    /**
     * @brief Selects a random transition shader.
     * @return A shared pointer to a transition shader, or an empty pointer if none are available.
     */
    auto RandomTransition() -> std::shared_ptr<Shader>;

    /**
     * @brief Returns a transition shader by name: the filename, without path or extension.
     *
     * Case-insensitive. This is what a preset-declared transition would resolve through.
     *
     * @param name The transition name, e.g. "ZoomBlur".
     * @return The shader, or an empty pointer if no transition of that name is loaded.
     */
    auto TransitionByName(const std::string& name) -> std::shared_ptr<Shader>;

private:
    /**
     * @brief Compiles a single transition shader program.
     * @param shaderBodyCode The mainImage() fragment shader code, without any headers etc.
     */
    static auto CompileTransitionShader(const std::string& shaderBodyCode) -> std::shared_ptr<Shader>;

    /**
     * @brief FileScanner callback: compiles one transition shader file and files it under its name.
     */
    void AddTransitionFile(const std::string& path, const std::string& basename);

    std::vector<std::shared_ptr<Shader>> m_builtInShaders; //!< Compiled-in transitions; the fallback.
    std::vector<std::shared_ptr<Shader>> m_loadedShaders;  //!< Transitions compiled from the transitions directory.
    std::map<std::string, std::shared_ptr<Shader>> m_shadersByName; //!< Loaded transitions, keyed by lower-cased name.

    std::random_device m_randomDevice; //!< Seed for the random number generator
    std::mt19937 m_mersenneTwister; //!< Random engine to select shader
};

} // namespace Renderer
} // namespace libprojectM
