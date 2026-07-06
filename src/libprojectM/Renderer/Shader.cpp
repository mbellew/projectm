#include "Shader.hpp"

#include <Logging.hpp>
#include <glm/gtc/type_ptr.hpp>

#include <atomic>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <vector>

namespace libprojectM {
namespace Renderer {

namespace {
// Prefix each line of GLSL source with its line number, so the driver's
// "ERROR: 0:NN" diagnostics can be mapped directly to a source line.
auto NumberSourceLines(const std::string& source) -> std::string
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

// --- Compiled-program disk cache (opt-in) -----------------------------------
//
// Linking a preset's transpiled GLSL is the dominant per-load shader cost. When
// PROJECTM_SHADER_CACHE points at a directory, the linked program is saved there
// via glGetProgramBinary keyed on the exact GLSL, and reloaded via glProgramBinary
// on the next run instead of being recompiled. The cache is purely an optimization:
// any miss, mismatch, or driver rejection falls back to a normal compile+link, so
// it can never produce a wrong or broken shader. Wipe the directory any time to
// reset it.
//
// Program binaries are driver/GPU-specific and version-fragile, so the on-disk
// header records a fingerprint of the GL context plus per-source hashes; a stale
// entry (driver update, edited GLSL) fails verification and is rebuilt.

constexpr std::uint32_t kCacheMagic = 0x314253504Du; // truncated 'PMSB1' marker
constexpr std::uint32_t kCacheSchema = 1;

struct CacheHeader
{
    std::uint32_t magic{};
    std::uint32_t schema{};
    std::uint64_t contextHash{};
    std::uint64_t vertexHash{};
    std::uint64_t fragmentHash{};
    std::uint32_t vertexLength{};
    std::uint32_t fragmentLength{};
    std::uint32_t binaryFormat{};
    std::uint32_t binaryLength{};
};

// 64-bit FNV-1a. Stable across runs and platforms; collisions are made
// irrelevant by also verifying source lengths in the cache header.
auto Fnv1a(const std::string& data, std::uint64_t seed = 0xcbf29ce484222325ull) -> std::uint64_t
{
    std::uint64_t hash = seed;
    for (unsigned char byte : data)
    {
        hash ^= byte;
        hash *= 0x100000001b3ull;
    }
    return hash;
}

// Returns the configured cache directory, or empty if caching is disabled.
auto ShaderCacheDirectory() -> std::string
{
    const char* dir = std::getenv("PROJECTM_SHADER_CACHE");
    return (dir != nullptr) ? std::string(dir) : std::string();
}

// A fingerprint of everything outside the GLSL that can change the produced
// binary: GL vendor/renderer/version and the program-binary format. A driver
// update or different GPU shifts this and self-invalidates stale entries.
auto GlContextHash() -> std::uint64_t
{
    auto glString = [](GLenum name) -> std::string {
        const auto* value = reinterpret_cast<const char*>(glGetString(name));
        return value != nullptr ? std::string(value) : std::string();
    };
    std::string fingerprint = glString(GL_VENDOR) + '\0' +
                              glString(GL_RENDERER) + '\0' +
                              glString(GL_VERSION) + '\0' +
                              glString(GL_SHADING_LANGUAGE_VERSION);
    return Fnv1a(fingerprint);
}

auto CacheFilePath(const std::string& cacheDir,
                   std::uint64_t contextHash,
                   std::uint64_t vertexHash,
                   std::uint64_t fragmentHash) -> std::filesystem::path
{
    std::ostringstream name;
    name << std::hex << std::setw(16) << std::setfill('0') << (contextHash ^ (vertexHash * 31) ^ fragmentHash)
         << '-' << std::setw(16) << vertexHash << std::setw(16) << fragmentHash << ".binshader";
    return std::filesystem::path(cacheDir) / name.str();
}

// Program binaries require the desktop GL_ARB_get_program_binary entry points
// (core in GL 4.1 / GLES 3). Returns false on contexts that lack them.
auto ProgramBinarySupported() -> bool
{
    // Apple's GL exposes the entry points but reports zero usable binary formats,
    // so this correctly disables the cache on macOS while enabling it elsewhere.
    GLint formatCount = 0;
    glGetIntegerv(GL_NUM_PROGRAM_BINARY_FORMATS, &formatCount);
    return glGetProgramBinary != nullptr && glProgramBinary != nullptr &&
           glProgramParameteri != nullptr && formatCount > 0;
}

// Session-wide cache observability. The feature is untested on Apple GL (zero
// binary formats disables it there); these logs let the Linux/NVIDIA target
// confirm the cache actually engaged and is hitting rather than always missing.
std::atomic<int> g_cacheHits{0};
std::atomic<int> g_cacheMisses{0};

// Log the enable/disable decision exactly once per process, only when the user
// requested caching (PROJECTM_SHADER_CACHE set), so it stays silent by default
// but explains itself on the machines where it matters.
void LogCacheStatusOnce(const std::string& cacheDir, bool supported)
{
    static std::atomic<bool> logged{false};
    if (logged.exchange(true))
    {
        return;
    }
    if (supported)
    {
        LOG_INFO("[Shader] Program binary cache enabled at \"" + cacheDir + "\"");
    }
    else
    {
        LOG_INFO("[Shader] PROJECTM_SHADER_CACHE set but disabled: the GL driver reports "
                 "zero program binary formats (expected on Apple GL).");
    }
}

// Referenced only from LOG_DEBUG, which compiles to nothing in release builds.
[[maybe_unused]] auto CacheCountsSuffix() -> std::string
{
    return " (" + std::to_string(g_cacheHits.load()) + " hits / " +
           std::to_string(g_cacheMisses.load()) + " misses)";
}
} // anonymous namespace

Shader::Shader()
    : m_shaderProgram(glCreateProgram())
{
}

Shader::~Shader()
{
    if (m_shaderProgram)
    {
        glDeleteProgram(m_shaderProgram);
    }
}

void Shader::CompileProgram(const std::string& vertexShaderSource,
                            const std::string& fragmentShaderSource)
{
    const std::string cacheDir = ShaderCacheDirectory();
    const bool cacheRequested = !cacheDir.empty();
    const bool cacheEnabled = cacheRequested && ProgramBinarySupported();
    if (cacheRequested)
    {
        LogCacheStatusOnce(cacheDir, cacheEnabled);
    }

    std::uint64_t contextHash{};
    std::uint64_t vertexHash{};
    std::uint64_t fragmentHash{};
    std::filesystem::path cacheFile;
    if (cacheEnabled)
    {
        contextHash = GlContextHash();
        vertexHash = Fnv1a(vertexShaderSource);
        fragmentHash = Fnv1a(fragmentShaderSource);
        cacheFile = CacheFilePath(cacheDir, contextHash, vertexHash, fragmentHash);

        if (LoadCachedProgram(cacheFile, contextHash, vertexHash, fragmentHash,
                              vertexShaderSource, fragmentShaderSource))
        {
            g_cacheHits.fetch_add(1);
            LOG_DEBUG("[Shader] Program binary cache hit" + CacheCountsSuffix());
            return;
        }

        g_cacheMisses.fetch_add(1);

        // Ask the driver to keep a retrievable binary so we can save it after linking.
        glProgramParameteri(m_shaderProgram, GL_PROGRAM_BINARY_RETRIEVABLE_HINT, GL_TRUE);
    }

    auto vertexShader = CompileShader(vertexShaderSource, GL_VERTEX_SHADER);
    auto fragmentShader = CompileShader(fragmentShaderSource, GL_FRAGMENT_SHADER);

    glAttachShader(m_shaderProgram, vertexShader);
    glAttachShader(m_shaderProgram, fragmentShader);

    glLinkProgram(m_shaderProgram);

    // Shader objects are no longer needed after linking, free the memory.
    glDetachShader(m_shaderProgram, vertexShader);
    glDetachShader(m_shaderProgram, fragmentShader);
    glDeleteShader(vertexShader);
    glDeleteShader(fragmentShader);

    GLint programLinked;
    glGetProgramiv(m_shaderProgram, GL_LINK_STATUS, &programLinked);
    if (programLinked == GL_TRUE)
    {
        if (cacheEnabled)
        {
            SaveCachedProgram(cacheFile, contextHash, vertexHash, fragmentHash,
                              vertexShaderSource, fragmentShaderSource);
            LOG_DEBUG("[Shader] Program binary cache miss; compiled and saved" + CacheCountsSuffix());
        }
        return;
    }

    GLint infoLogLength{};
    glGetProgramiv(m_shaderProgram, GL_INFO_LOG_LENGTH, &infoLogLength);
    std::vector<char> message(infoLogLength + 1);
    glGetProgramInfoLog(m_shaderProgram, infoLogLength, nullptr, message.data());

    std::string linkError = "[Shader] Error linking compiled shader program: " + std::string(message.data());
    LOG_ERROR(linkError);
    LOG_ERROR("[Shader] Vertex shader source (line-numbered):\n" + NumberSourceLines(vertexShaderSource));
    LOG_ERROR("[Shader] Fragment shader source (line-numbered):\n" + NumberSourceLines(fragmentShaderSource));
    throw ShaderException(linkError);
}

bool Shader::LoadCachedProgram(const std::filesystem::path& cacheFile,
                               std::uint64_t contextHash,
                               std::uint64_t vertexHash,
                               std::uint64_t fragmentHash,
                               const std::string& vertexShaderSource,
                               const std::string& fragmentShaderSource)
{
    std::ifstream file(cacheFile, std::ios::binary);
    if (!file)
    {
        return false;
    }

    CacheHeader header{};
    file.read(reinterpret_cast<char*>(&header), sizeof(header));
    if (!file ||
        header.magic != kCacheMagic ||
        header.schema != kCacheSchema ||
        header.contextHash != contextHash ||
        header.vertexHash != vertexHash ||
        header.fragmentHash != fragmentHash ||
        header.vertexLength != vertexShaderSource.size() ||
        header.fragmentLength != fragmentShaderSource.size() ||
        header.binaryLength == 0)
    {
        return false;
    }

    std::vector<char> binary(header.binaryLength);
    file.read(binary.data(), static_cast<std::streamsize>(binary.size()));
    if (!file)
    {
        return false;
    }

    glProgramBinary(m_shaderProgram, header.binaryFormat, binary.data(),
                    static_cast<GLsizei>(binary.size()));

    // The driver can reject a stale binary; it signals this via the link status
    // rather than an error, so we must check and fall back to a fresh compile.
    GLint programLinked{GL_FALSE};
    glGetProgramiv(m_shaderProgram, GL_LINK_STATUS, &programLinked);
    return programLinked == GL_TRUE;
}

void Shader::SaveCachedProgram(const std::filesystem::path& cacheFile,
                               std::uint64_t contextHash,
                               std::uint64_t vertexHash,
                               std::uint64_t fragmentHash,
                               const std::string& vertexShaderSource,
                               const std::string& fragmentShaderSource) const
{
    GLint binaryLength = 0;
    glGetProgramiv(m_shaderProgram, GL_PROGRAM_BINARY_LENGTH, &binaryLength);
    if (binaryLength <= 0)
    {
        return;
    }

    std::vector<char> binary(static_cast<size_t>(binaryLength));
    GLenum binaryFormat = 0;
    GLsizei written = 0;
    glGetProgramBinary(m_shaderProgram, binaryLength, &written, &binaryFormat, binary.data());
    if (written <= 0)
    {
        return;
    }

    CacheHeader header{};
    header.magic = kCacheMagic;
    header.schema = kCacheSchema;
    header.contextHash = contextHash;
    header.vertexHash = vertexHash;
    header.fragmentHash = fragmentHash;
    header.vertexLength = static_cast<std::uint32_t>(vertexShaderSource.size());
    header.fragmentLength = static_cast<std::uint32_t>(fragmentShaderSource.size());
    header.binaryFormat = static_cast<std::uint32_t>(binaryFormat);
    header.binaryLength = static_cast<std::uint32_t>(written);

    std::error_code ec;
    std::filesystem::create_directories(cacheFile.parent_path(), ec);

    // Write to a temp file and rename so a second instance never sees a partial file.
    std::filesystem::path tempFile = cacheFile;
    tempFile += ".tmp";
    {
        std::ofstream file(tempFile, std::ios::binary | std::ios::trunc);
        if (!file)
        {
            return;
        }
        file.write(reinterpret_cast<const char*>(&header), sizeof(header));
        file.write(binary.data(), static_cast<std::streamsize>(written));
        if (!file)
        {
            file.close();
            std::filesystem::remove(tempFile, ec);
            return;
        }
    }
    std::filesystem::rename(tempFile, cacheFile, ec);
    if (ec)
    {
        std::filesystem::remove(tempFile, ec);
    }
}

bool Shader::Validate(std::string& validationMessage) const
{
    GLint result{GL_FALSE};
    int infoLogLength;

    glValidateProgram(m_shaderProgram);

    glGetProgramiv(m_shaderProgram, GL_VALIDATE_STATUS, &result);
    glGetProgramiv(m_shaderProgram, GL_INFO_LOG_LENGTH, &infoLogLength);
    if (infoLogLength > 0)
    {
        std::vector<char> validationErrorMessage(infoLogLength + 1);
        glGetProgramInfoLog(m_shaderProgram, infoLogLength, nullptr, validationErrorMessage.data());
        validationMessage = std::string(validationErrorMessage.data());
    }

    return result;
}

void Shader::Bind() const
{
    if (m_shaderProgram > 0)
    {
        glUseProgram(m_shaderProgram);
    }
}

void Shader::Unbind()
{
    glUseProgram(0);
}

void Shader::SetUniformFloat(const char* uniform, float value) const
{
    auto location = glGetUniformLocation(m_shaderProgram, uniform);
    if (location < 0)
    {
        return;
    }
    glUniform1fv(location, 1, &value);
}

void Shader::SetUniformInt(const char* uniform, int value) const
{
    auto location = glGetUniformLocation(m_shaderProgram, uniform);
    if (location < 0)
    {
        return;
    }
    glUniform1iv(location, 1, &value);
}

void Shader::SetUniformFloat2(const char* uniform, const glm::vec2& values) const
{
    auto location = glGetUniformLocation(m_shaderProgram, uniform);
    if (location < 0)
    {
        return;
    }
    glUniform2fv(location, 1, glm::value_ptr(values));
}

void Shader::SetUniformInt2(const char* uniform, const glm::ivec2& values) const
{
    auto location = glGetUniformLocation(m_shaderProgram, uniform);
    if (location < 0)
    {
        return;
    }
    glUniform2iv(location, 1, glm::value_ptr(values));
}

void Shader::SetUniformFloat3(const char* uniform, const glm::vec3& values) const
{
    auto location = glGetUniformLocation(m_shaderProgram, uniform);
    if (location < 0)
    {
        return;
    }
    glUniform3fv(location, 1, glm::value_ptr(values));
}

void Shader::SetUniformInt3(const char* uniform, const glm::ivec3& values) const
{
    auto location = glGetUniformLocation(m_shaderProgram, uniform);
    if (location < 0)
    {
        return;
    }
    glUniform3iv(location, 1, glm::value_ptr(values));
}

void Shader::SetUniformFloat4(const char* uniform, const glm::vec4& values) const
{
    auto location = glGetUniformLocation(m_shaderProgram, uniform);
    if (location < 0)
    {
        return;
    }
    glUniform4fv(location, 1, glm::value_ptr(values));
}

void Shader::SetUniformInt4(const char* uniform, const glm::ivec4& values) const
{
    auto location = glGetUniformLocation(m_shaderProgram, uniform);
    if (location < 0)
    {
        return;
    }
    glUniform4iv(location, 1, glm::value_ptr(values));
}

void Shader::SetUniformMat3x4(const char* uniform, const glm::mat3x4& values) const
{
    auto location = glGetUniformLocation(m_shaderProgram, uniform);
    if (location < 0)
    {
        return;
    }
    glUniformMatrix3x4fv(location, 1, GL_FALSE, glm::value_ptr(values));
}

void Shader::SetUniformMat4x4(const char* uniform, const glm::mat4x4& values) const
{
    auto location = glGetUniformLocation(m_shaderProgram, uniform);
    if (location < 0)
    {
        return;
    }
    glUniformMatrix4fv(location, 1, GL_FALSE, glm::value_ptr(values));
}

GLuint Shader::CompileShader(const std::string& source, GLenum type)
{
    GLint shaderCompiled{};

    auto shader = glCreateShader(type);
    const auto* shaderSourceCStr = source.c_str();
    glShaderSource(shader, 1, &shaderSourceCStr, nullptr);

    glCompileShader(shader);

    glGetShaderiv(shader, GL_COMPILE_STATUS, &shaderCompiled);
    if (shaderCompiled == GL_TRUE)
    {
        return shader;
    }

    GLint infoLogLength{};
    glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &infoLogLength);
    std::vector<char> message(infoLogLength + 1);
    glGetShaderInfoLog(shader, infoLogLength, nullptr, message.data());
    glDeleteShader(shader);

    std::string compileError = "[Shader] Error compiling " + std::string(type == GL_VERTEX_SHADER ? "vertex" : "fragment") + " shader: " + std::string(message.data());
    LOG_ERROR(compileError);
    LOG_ERROR("[Shader] Failed source (line numbers match the driver's \"0:NN\" references):\n" + NumberSourceLines(source));
    throw ShaderException(compileError);
}

auto Shader::GetShaderLanguageVersion() -> Shader::GlslVersion
{
    const char* shaderLanguageVersion = reinterpret_cast<const char*>(glGetString(GL_SHADING_LANGUAGE_VERSION));

    if (shaderLanguageVersion == nullptr)
    {
        return {};
    }

    std::string shaderLanguageVersionString(shaderLanguageVersion);

    // Some OpenGL implementations add non-standard-conforming text in front, e.g. WebGL, which returns "OpenGL ES GLSL ES 3.00 ..."
    // Find the first digit and start there.
    auto firstDigit = shaderLanguageVersionString.find_first_of("0123456789");
    if (firstDigit != std::string::npos && firstDigit != 0)
    {
        shaderLanguageVersionString = shaderLanguageVersionString.substr(firstDigit);
    }

    // Cut off the vendor-specific information, if any
    auto spacePos = shaderLanguageVersionString.find(' ');
    if (spacePos != std::string::npos)
    {
        shaderLanguageVersionString.resize(spacePos);
    }

    auto dotPos = shaderLanguageVersionString.find('.');
    if (dotPos == std::string::npos)
    {
        return {};
    }

    int versionMajor = std::stoi(shaderLanguageVersionString.substr(0, dotPos));
    int versionMinor = std::stoi(shaderLanguageVersionString.substr(dotPos + 1));

    return {versionMajor, versionMinor};
}

} // namespace Renderer
} // namespace libprojectM
