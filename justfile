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

# Build a self-contained, relocatable macOS appliance bundle into dist/ (see deploy/README.md)
deploy-macos: build
    BUILD_DIR={{build_dir}} DEPTHAI_PREFIX={{depthai_prefix}} ONNX_PREFIX={{onnx_prefix}} \
        deploy/deploy-macos.sh

# Build a self-contained Linux appliance bundle (not yet implemented)
deploy-linux: build
    BUILD_DIR={{build_dir}} DEPTHAI_PREFIX={{depthai_prefix}} ONNX_PREFIX={{onnx_prefix}} \
        deploy/deploy-linux.sh

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
