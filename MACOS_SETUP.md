# macOS Development Setup

How to build and run this projectM fork on macOS (Apple Silicon), including the optional
camera / person-segmentation features and the runtime assets they need. Tested on macOS 15
(Apple Silicon, AppleClang) with Homebrew.

> This documents the per-machine setup that is **not** captured by the repo itself (Homebrew
> packages, out-of-tree libraries installed under `~/.local`, and runtime assets under
> `~/.projectM`). For the official cross-platform instructions see [BUILDING.md](BUILDING.md)
> and [BUILDING-cmake.md](BUILDING-cmake.md); for local quirks see
> [CLAUDE_local.md](CLAUDE_local.md).

---

## 1. Quick start (core build, no camera features)

```bash
brew install cmake sdl2 just
git clone <this-repo> projectm && cd projectm
git submodule update --init --recursive

mkdir -p cmake-build && cd cmake-build
cmake -DCMAKE_BUILD_TYPE=Release -DENABLE_SDL_UI=ON ..
cmake --build . --parallel
./src/sdl-test-ui/projectM-Test-UI
```

- `ENABLE_SDL_UI=ON` is required to build the test UI (off by default).
- The binary is `projectM-Test-UI` (some docs call it `projectMSDL` — that name is stale).

The default `just build` (see §6) goes further and enables the **camera person-seg** and
**Luxonis OAK** features, which need the extra libraries in §3. The plain build above skips both.

---

## 2. Homebrew packages

| Package      | Why                                                                  |
|--------------|----------------------------------------------------------------------|
| `cmake`      | Build system.                                                        |
| `sdl2`       | Window / GL context / audio + video capture in the test UI.          |
| `just`       | Task runner (`just build`, `just run`, …) — see [justfile](justfile).|
| `ansible`    | Only for the **appliance** kiosk provisioning (`appliance/`).         |
| `openssl@3`  | Only for `deploy/make-signing-cert.sh` *generation* — but note the **system** `/usr/bin/openssl` (LibreSSL) is what actually creates the p12 there (Homebrew OpenSSL 3.x p12s won't import into the keychain). |
| `libusb`     | Pulled in by depthai-core (OAK USB). Usually transitive.             |

```bash
brew install cmake sdl2 just ansible
```

> **SDL2 include-path pitfall (already patched here).** Homebrew's SDL2 CMake config points
> `INTERFACE_INCLUDE_DIRECTORIES` at `…/include/SDL2`, but the code uses `#include <SDL2/SDL.h>`,
> producing `…/SDL2/SDL2/SDL.h`. The repo's [cmake/SDL2Target.cmake](cmake/SDL2Target.cmake) strips
> the trailing `/SDL2`. If you ever see "SDL.h not found", confirm that fix is present (it must
> **not** be gated on `_SDL2_TARGET_TYPE`, which is never set).

---

## 3. Optional libraries (camera features) — installed under `~/.local`

These live outside the repo and are referenced by `CMAKE_PREFIX_PATH`. The `just build` target
expects both at the prefixes below; if either is missing, CMake prints a warning and builds a
**no-op stub**, so the app still links — the feature is just inert.

### 3a. ONNX Runtime — person segmentation (`ENABLE_ONNX_SEG`)

Foreground person-matting on the webcam (`Video Mask = seg`). Uses the **CoreML** execution
provider (Apple Neural Engine).

1. Download the prebuilt **`onnxruntime-osx-arm64-<version>.tgz`** from
   <https://github.com/microsoft/onnxruntime/releases> (this machine runs **1.26.0**).
2. Extract it to `~/.local/onnxruntime` so you have `~/.local/onnxruntime/{include,lib}`:
   ```bash
   mkdir -p ~/.local
   tar -xzf onnxruntime-osx-arm64-1.26.0.tgz
   mv onnxruntime-osx-arm64-1.26.0 ~/.local/onnxruntime
   ```

> The prebuilt package ships a CMake config, but its imported target points at a non-existent
> `include/onnxruntime` subdir — so this project locates the headers/lib with `find_path` /
> `find_library` instead. Just put it on `CMAKE_PREFIX_PATH` (the justfile does this) and it's
> found.

### 3b. depthai-core — Luxonis OAK depth camera (`ENABLE_LUXONIS`, optional)

Only needed if you use an **OAK-D** depth camera. **Cannot** be consumed via FetchContent
(depthai uses the Hunter package manager, whose `HunterGate` must run before `project()`).
Build & install it standalone, then `find_package(depthai)`. This host uses **v2.30.0**.

```bash
git clone --recursive -b v2.30.0 https://github.com/luxonis/depthai-core
```

On a bleeding-edge toolchain (Homebrew CMake 4.x + AppleClang 21 + modern SDK) two things break,
so use a toolchain shim and a policy env var:

```bash
# /tmp/depthai-toolchain.cmake — reaches every nested Hunter sub-build
cat > /tmp/depthai-toolchain.cmake <<'EOF'
# zlib 1.2.11 #define fdopen NULL under TARGET_OS_MAC clobbers the SDK fdopen;
# the identity macro makes its `#ifndef fdopen` skip the bad define. Also relax
# clang-21's now-hard implicit-function-declaration / non-prototype errors.
set(CMAKE_C_FLAGS_INIT "-Dfdopen=fdopen -Wno-error=implicit-function-declaration -Wno-implicit-function-declaration -Wno-error=implicit-int -Wno-deprecated-non-prototype")
set(CMAKE_CXX_FLAGS_INIT "-Wno-deprecated-declarations")
EOF

# Must be an ENV var (not -D): it has to propagate to Hunter's nested cmake calls.
export CMAKE_POLICY_VERSION_MINIMUM=3.5

cmake -S depthai-core -B /tmp/depthai-build \
  -DCMAKE_TOOLCHAIN_FILE=/tmp/depthai-toolchain.cmake \
  -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$HOME/.local/depthai-core \
  -DBUILD_SHARED_LIBS=ON -DDEPTHAI_OPENCV_SUPPORT=OFF \
  -DDEPTHAI_BUILD_EXAMPLES=OFF -DDEPTHAI_BUILD_TESTS=OFF
cmake --build /tmp/depthai-build --target install --parallel
```

- `DEPTHAI_OPENCV_SUPPORT=OFF` skips a heavy OpenCV Hunter build (`depthai::core` doesn't need it).
- Hunter's download cache lives in `~/.hunter` (large; safe to delete to reclaim space).
- If you have no OAK device, skip this and build with `-DENABLE_LUXONIS=OFF`.

---

## 4. Runtime assets under `~/.projectM`

These are **not** in the repo. Create the directory tree and populate it:

```
~/.projectM/
  config.inp          # settings (see §5)
  models/             # ONNX person-seg models (§4a)
  textures/           # image samplers for presets (§4b)
  palettes/           # image-file color palettes (§4c)
```

### 4a. `models/` — ONNX person-seg models

Only needed for `Video Mask = seg`. Drop the `.onnx` files here. The default is RVM. **Check each
project's repository for its license before redistributing the weights.**

| File                     | Size  | Model / source                                                     |
|--------------------------|-------|--------------------------------------------------------------------|
| `rvm_mobilenetv3.onnx`   | ~15MB | Robust Video Matting (`rvm_mobilenetv3_fp32.onnx`), [PeterL1n/RobustVideoMatting](https://github.com/PeterL1n/RobustVideoMatting) — real-time, temporally stable; **default** |
| `u2net_human_seg.onnx`   | ~176MB| U²-Net human segmentation, from [xuebinqin/U-2-Net](https://github.com/xuebinqin/U-2-Net) (human-seg model, ONNX-converted) — class-aware, rejects background objects |
| `modnet.onnx`            | ~26MB | MODNet portrait matting ([ZHKKKe/MODNet](https://github.com/ZHKKKe/MODNet), ONNX export) — good matte but slower on ANE |

- `coreml_cache/` appears automatically (CoreML compiles the model on first run; cached after).
- Model is auto-detected by family: RVM (has a `downsample_ratio` input), U²-Net (filename contains
  `u2net`), else single-input MODNet. Pick one with `Video Seg Model` / `$PROJECTM_SEG_MODEL`.

### 4b. `textures/` — image samplers

Image files (`.jpg`/`.png`/…) that presets reference via image samplers (`sampler_xyz`). Point the
app at them with `Texture Path` in `config.inp` (default `~/.projectM/textures`). The classic
**MilkDrop "texture pack"** plus any custom images go here. Presets that reference a missing
texture fail to compile their warp/comp shaders, so a populated `textures/` matters.

### 4c. `palettes/` — image-file color palettes

Image files for the `PALETTE_NAME` preset key (a preset's `PALETTE_NAME=foo` loads `foo.{png,jpg,…}`
here, else a built-in family). Point the app at them with `Palette Path` in `config.inp`. See the
palette test preset `presets/tests/604-palette-image.milk`.

---

## 5. `~/.projectM/config.inp`

Local, untracked settings file (auto-seeded from the data dir on first run). Common keys:

```ini
Mesh X = 48
Mesh Y = 32
FPS = 30
Fullscreen = false

# ';'-separated, case-insensitive name substrings; first present device wins, else system default.
Audio Devices = BlackHole 2ch
Video Devices = Camera

# Foreground masking: off | source | motion | decay | chroma | seg ...
Video Mask = seg
Video Seg Quality = 2          # 1/2/3 -> 256/384/512 model input; bigger = crisper but slower

# Asset search paths ("~" expands to $HOME).
Texture Path = ~/.projectM/textures
Palette Path = ~/.projectM/palettes
```

Most keys also have `$PROJECTM_*` env overrides (e.g. `PROJECTM_VIDEO_MASK`, `PROJECTM_SEG_QUALITY`,
`PROJECTM_PRESET_PATH`).

---

## 6. Build & run (just)

The [justfile](justfile) drives the full-featured build (camera seg + OAK on, prefixes wired):

```bash
just build          # configure + build into cmake-build-luxonis/
just run            # build + run on presets/cream-of-the-crop
just video          # run on presets/video
just preset presets/tests/604-palette-image.milk   # run one preset
```

`just build` is equivalent to:

```bash
cmake -DCMAKE_BUILD_TYPE=Release -DENABLE_SDL_UI=ON \
      -DENABLE_LUXONIS=ON -DENABLE_ONNX_SEG=ON \
      -DCMAKE_PREFIX_PATH="$HOME/.local/depthai-core;$HOME/.local/onnxruntime" \
      -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
```

To build **without** the camera libraries, drop `-DENABLE_LUXONIS`/`-DENABLE_ONNX_SEG` (or just
use the §1 plain build); the seg/OAK paths become no-op stubs.

---

## 7. Camera / microphone permission (TCC) — pitfall

The seg feature and all video presets need camera access, which macOS gates per-user via TCC:

- A **GUI login session** is required. Running the binary via `ssh` or `sudo su <user>` runs it
  inside the *wrong* session — the camera silently yields black frames (the app still logs
  "Selected video device"). Log in at the desktop as that user and launch it there.
- First launch prompts for camera + microphone — click **Allow**. The grant is keyed to the
  binary's code-signing identity; an **ad-hoc** signature changes every rebuild, so the grant
  doesn't persist. For an appliance, sign with a stable identity — see `deploy/make-signing-cert.sh`
  and [deploy/README.md](deploy/README.md).

---

## 8. Packaging / appliance

- `just deploy-macos` builds a self-contained, relocatable bundle in `dist/projectm` (binary +
  bundled dylibs + presets + palettes + a `~/.projectM` seed). See [deploy/README.md](deploy/README.md).
- `appliance/` provisions a kiosk (auto-login + autostart LaunchAgent) via Ansible. See
  [appliance/README.md](appliance/README.md).
