# Build directory, per-OS so the macOS Luxonis build and the Linux V4L2 build don't clobber each other.
build_dir := if os() == "macos" { "cmake-build-luxonis" } else { "cmake-build-linux" }
# Luxonis OAK (depthai-core) install prefix; see reference_depthai_build_macos for how it's built.
depthai_prefix := env_var('HOME') / ".local/depthai-core"
# ONNX Runtime (person-seg) install prefix (prebuilt: osx-arm64 on macOS, linux-x64 on Linux).
onnx_prefix := env_var('HOME') / ".local/onnxruntime"

# Audio/video source preferences (and other settings) live in ~/.projectM/config.inp,
# a local, untracked file: Audio Devices / Video Devices / Fullscreen, etc.

# Configure and build with ONNX person-seg support.
#   macOS: also enables the Luxonis OAK depth camera (depthai-core).
#   Linux: enables the V4L2 camera backend instead (Luxonis not wired up yet).
build:
    #!/usr/bin/env bash
    set -euo pipefail
    if [ "{{os()}}" = "macos" ]; then
        cmake -S . -B {{build_dir}} -DCMAKE_BUILD_TYPE=Release -DENABLE_SDL_UI=ON \
            -DENABLE_LUXONIS=ON -DENABLE_ONNX_SEG=ON \
            -DCMAKE_PREFIX_PATH="{{depthai_prefix}};{{onnx_prefix}}" \
            -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
    else
        cmake -S . -B {{build_dir}} -DCMAKE_BUILD_TYPE=Release -DENABLE_SDL_UI=ON \
            -DENABLE_VIDEO_CAPTURE=ON -DENABLE_ONNX_SEG=ON \
            -DCMAKE_PREFIX_PATH="{{onnx_prefix}}" \
            -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
    fi
    cmake --build {{build_dir}} --parallel

# Run the test UI
run: build
    PROJECTM_PRESET_PATH=presets/cream-of-the-crop {{build_dir}}/src/sdl-test-ui/projectM-Test-UI

# Run the test UI with presets/tests as the preset path
test: build
    PROJECTM_PRESET_PATH=presets/tests {{build_dir}}/src/sdl-test-ui/projectM-Test-UI

# Run the test UI with presets/video as the preset path
video: build
    PROJECTM_PRESET_PATH=presets/video {{build_dir}}/src/sdl-test-ui/projectM-Test-UI

# Run the test UI with favorites.txt as the preset list
favorites: build
    PROJECTM_PRESET_LIST=favorites.txt {{build_dir}}/src/sdl-test-ui/projectM-Test-UI

# Run the test UI with videos.txt as the preset list
videotxt: build
    PROJECTM_PRESET_LIST=video.txt {{build_dir}}/src/sdl-test-ui/projectM-Test-UI


# Run the test UI on a single preset, e.g. `just preset presets/tests/402-compshader-video-motion.milk`
preset file: build
    #!/usr/bin/env bash
    set -euo pipefail
    list="$(mktemp -t projectm-preset.XXXXXX)"
    trap 'rm -f "$list"' EXIT
    printf '%s\n' "{{file}}" > "$list"
    PROJECTM_PRESET_LIST="$list" {{build_dir}}/src/sdl-test-ui/projectM-Test-UI

# Build a self-contained, relocatable macOS appliance bundle into dist/ (see deploy/README.md)
deploy-macos: build
    BUILD_DIR={{build_dir}} DEPTHAI_PREFIX={{depthai_prefix}} ONNX_PREFIX={{onnx_prefix}} \
        deploy/deploy-macos.sh

# Build a self-contained Linux appliance bundle (not yet implemented)
deploy-linux: build
    BUILD_DIR={{build_dir}} DEPTHAI_PREFIX={{depthai_prefix}} ONNX_PREFIX={{onnx_prefix}} \
        deploy/deploy-linux.sh
