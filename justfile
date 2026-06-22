# Build directory, per-OS so the macOS Luxonis build and the Linux V4L2 build don't clobber each other.
build_dir := if os() == "macos" { "cmake-build-luxonis" } else { "cmake-build-linux" }
# Luxonis OAK (depthai-core) install prefix; see reference_depthai_build_macos for how it's built.
depthai_prefix := env_var('HOME') / ".local/depthai-core"
# ONNX Runtime (person-seg) install prefix (prebuilt: osx-arm64 on macOS, linux-x64 on Linux).
onnx_prefix := env_var('HOME') / ".local/onnxruntime"
onnx_lib_dir := onnx_prefix / "lib"
# CUDA 13 / cuDNN 9 runtime libs for the GPU ONNX Runtime person-seg (see LINUX_SETUP.md §9).
cuda_runtime_dir := env_var('HOME') / ".local/cuda-runtime/lib"

# Inline env prefix for the run recipes: puts the ONNX Runtime + CUDA runtime libs on the loader
# path so the seg model runs on the GPU (CUDA execution provider). Without it the CUDA provider
# can't dlopen cuBLAS/cuDNN and seg silently falls back to the CPU — which a profile shows is ~80%
# of CPU time. Set INLINE on the command (not via `export`) on purpose: snap-packaged `just` strips
# an exported LD_LIBRARY_PATH for security, but an inline assignment applied by the recipe shell at
# exec time survives. Empty on macOS (CoreML EP; finds its libs via rpath).
run_env := if os() == "linux" { "LD_LIBRARY_PATH=" + cuda_runtime_dir + ":" + onnx_lib_dir } else { "" }

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
    {{run_env}} PROJECTM_PRESET_PATH=presets/cream-of-the-crop {{build_dir}}/src/sdl-test-ui/projectM-Test-UI

# Run the test UI with presets/tests as the preset path
test: build
    {{run_env}} PROJECTM_PRESET_PATH=presets/tests {{build_dir}}/src/sdl-test-ui/projectM-Test-UI

# Run the test UI with presets/video as the preset path
video: build
    {{run_env}} PROJECTM_PRESET_PATH=presets/video {{build_dir}}/src/sdl-test-ui/projectM-Test-UI

# Run the test UI with favorites.txt as the preset list
favorites: build
    {{run_env}} PROJECTM_PRESET_LIST=favorites.txt {{build_dir}}/src/sdl-test-ui/projectM-Test-UI

# Run the test UI with videos.txt as the preset list
videotxt: build
    {{run_env}} PROJECTM_PRESET_LIST=video.txt {{build_dir}}/src/sdl-test-ui/projectM-Test-UI


# Run the test UI on a single preset, e.g. `just preset presets/tests/402-compshader-video-motion.milk`
preset file: build
    #!/usr/bin/env bash
    set -euo pipefail
    list="$(mktemp -t projectm-preset.XXXXXX)"
    trap 'rm -f "$list"' EXIT
    printf '%s\n' "{{file}}" > "$list"
    {{run_env}} PROJECTM_PRESET_LIST="$list" {{build_dir}}/src/sdl-test-ui/projectM-Test-UI

# Build a self-contained, relocatable macOS appliance bundle into dist/ (see deploy/README.md)
deploy-macos: build
    BUILD_DIR={{build_dir}} DEPTHAI_PREFIX={{depthai_prefix}} ONNX_PREFIX={{onnx_prefix}} \
        deploy/deploy-macos.sh

# Install the Linux appliance tree to /opt/projectm (binary + libs + CUDA runtime + models +
# presets). Needs sudo to write /opt; the source paths are passed explicitly so they survive sudo.
# Stage elsewhere without sudo by calling the script directly: deploy/deploy-linux.sh /tmp/projectm
deploy-linux: build
    sudo BUILD_DIR={{justfile_directory()}}/{{build_dir}} ONNX_PREFIX={{onnx_prefix}} \
        CUDA_RUNTIME_DIR={{cuda_runtime_dir}} MODELS_DIR={{env_var('HOME')}}/.projectM/models \
        TEXTURES_DIR={{env_var('HOME')}}/.projectM/textures \
        {{justfile_directory()}}/deploy/deploy-linux.sh

# Install the built macOS bundle into an appliance user's home + seed ~/.projectM (needs sudo).
# Run `just deploy-macos` first. Example: `just deploy-to-user brpl`
deploy-to-user user="brpl": deploy-macos
    sudo deploy/install-to-appliance.sh {{user}}

# Edit appliance/ansible/group_vars/all.yml first; prompts for sudo. Does NOT enable auto-login or
# rewrite config.inp. Then log in as the appliance user to verify (camera works in that user's GUI
# session; logs at ~<user>/.projectM/projectm.log).
# Install the appliance autostart LaunchAgent (projectM fullscreen at the appliance user's login)
appliance-autostart:
    cd appliance && ./bootstrap.sh --tags autostart

# Needs sudo + a real appliance_user_password in group_vars/all.yml; requires FileVault OFF. Reboot
# to take effect.
# Enable macOS auto-login for the appliance user (writes /etc/kcpassword)
appliance-autologin:
    cd appliance && ./bootstrap.sh --tags autologin

# Skip config when the deploy bundle already seeded ~/.projectM/config.inp: append `--skip-tags config`.
# Apply the full appliance playbook: user + config + autostart + autologin (see appliance/README.md)
appliance-provision:
    cd appliance && ./bootstrap.sh
