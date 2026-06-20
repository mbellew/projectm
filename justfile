build_dir := "cmake-build-luxonis"
# Luxonis OAK (depthai-core) install prefix; see reference_depthai_build_macos for how it's built.
depthai_prefix := env_var('HOME') / ".local/depthai-core"
# ONNX Runtime (person-seg) install prefix; prebuilt osx-arm64 package.
onnx_prefix := env_var('HOME') / ".local/onnxruntime"

# Audio/video source preferences (and other settings) live in ~/.projectM/config.inp,
# a local, untracked file: Audio Devices / Video Devices / Fullscreen, etc.

# Configure and build (Luxonis OAK depth + ONNX person-seg support enabled)
build:
    mkdir -p {{build_dir}}
    cd {{build_dir}} && cmake -DCMAKE_BUILD_TYPE=Release -DENABLE_SDL_UI=ON \
        -DENABLE_LUXONIS=ON -DENABLE_ONNX_SEG=ON \
        -DCMAKE_PREFIX_PATH="{{depthai_prefix}};{{onnx_prefix}}" \
        -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
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
