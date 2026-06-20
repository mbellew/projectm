# Linux Setup (Ubuntu 24.04) — building the `video_texture` branch with ONNX person-seg

This document records everything needed to reproduce a working Linux build of the
SDL test UI on the `video_texture` branch, **including** the optional ONNX
person-segmentation video-masking path (webcam → matte in alpha → preset samples it).

Reproduced on Ubuntu 24.04 (noble), GCC 13.3, CMake 3.28, NVIDIA GL 3.3 core.

---

## 1. Repositories enlisted

| What | URL | Ref |
|------|-----|-----|
| Main repo (`origin`) | https://github.com/mbellew/projectm | branch `video_texture` (tracks `origin/video_texture`) |
| Submodule `vendor/projectm-eval` | https://github.com/projectM-visualizer/projectm-eval.git | commit `78ccfc8` (v1.0.0-30) |

```shell
git clone https://github.com/mbellew/projectm
cd projectm
git switch video_texture
git submodule update --init --recursive    # pulls vendor/projectm-eval at the branch-pinned commit
```

> The submodule commit differs between `master` and `video_texture`; always re-run
> `git submodule update --init --recursive` after switching branches or it shows as
> modified (`M vendor/projectm-eval`).

Other dependencies (GLM, glad, hlslparser, stb_image) are **vendored** in `vendor/`
— no system packages needed for them. OpenGL itself is loaded at runtime via the
bundled `glad`, so **no GL/GLES dev headers are required** on Linux.

---

## 2. System packages (apt)

The base toolchain (`build-essential`, `cmake`, `git`, `curl`) is assumed present.
Only SDL2 (for the test UI) needs installing. V4L2 headers (`linux/videodev2.h`)
ship with the kernel headers already present on a standard Ubuntu install.

```shell
sudo apt install \
  libsdl2-dev \
  libsdl2-2.0-0=2.30.0+dfsg-1build3 \
  libpulse-dev \
  libpulse0=1:16.1+dfsg1-2ubuntu10 \
  libpulse-mainloop-glib0=1:16.1+dfsg1-2ubuntu10
```

**Why the version pins (apt version-skew gotcha):** `libsdl2-dev` (and transitively
`libpulse-dev`) hard-depend on an **exact** (`=`) version of their runtime libs. On
this box the runtime libs had been upgraded to the `noble-updates` versions, but the
matching `-dev` packages only exist at the base `noble` versions, so a plain
`apt install libsdl2-dev` fails with "unmet dependencies". Pinning the runtime libs
back to the base versions (above) satisfies the `=` constraints. The downgrade is
safe — every other consumer (gnome-shell, gstreamer, pipewire) uses loose `>=`
constraints. Installed versions after setup:

```
libsdl2-dev                2.30.0+dfsg-1build3
libsdl2-2.0-0              2.30.0+dfsg-1build3
libpulse-dev               1:16.1+dfsg1-2ubuntu10
libpulse0                  1:16.1+dfsg1-2ubuntu10
libpulse-mainloop-glib0    1:16.1+dfsg1-2ubuntu10
```

---

## 3. External downloads (not in any repo)

### ONNX Runtime (CPU, prebuilt) → `~/.local/onnxruntime`

```shell
ver=1.27.0
cd /tmp
curl -fsSLO https://github.com/microsoft/onnxruntime/releases/download/v${ver}/onnxruntime-linux-x64-${ver}.tgz
tar xzf onnxruntime-linux-x64-${ver}.tgz
mkdir -p ~/.local/onnxruntime
cp -r onnxruntime-linux-x64-${ver}/include ~/.local/onnxruntime/
cp -r onnxruntime-linux-x64-${ver}/lib     ~/.local/onnxruntime/
```

The build's `find_path`/`find_library` locate `onnxruntime_cxx_api.h` and
`libonnxruntime.so` under this prefix via `CMAKE_PREFIX_PATH` (see §5). CPU
execution provider only — the CoreML EP is macOS-only and is `#ifdef __APPLE__`'d
out (see §4).

### Person-seg model → `~/.projectM/models/rvm_mobilenetv3.onnx`

Robust Video Matting (mobilenetv3), the app's default model path:

```shell
mkdir -p ~/.projectM/models
curl -fsSL -o ~/.projectM/models/rvm_mobilenetv3.onnx \
  https://github.com/PeterL1n/RobustVideoMatting/releases/download/v1.0.0/rvm_mobilenetv3_fp32.onnx
```

`segMask.cpp` auto-detects RVM by its `downsample_ratio` input. Other supported
families: MODNet, U²-Net (`*u2net*` filename), YOLOv8/v11-seg (`*yolo*` filename).
Override the path with `$PROJECTM_SEG_MODEL`.

---

## 4. Source changes made for Linux (committed on `video_texture`)

1. **`src/sdl-test-ui/pmSDL.hpp`** — moved `#include <chrono>` / `<memory>` out of
   the `#ifdef PROJECTM_VIDEO_CAPTURE_ENABLED` guard. The `trackFrameRate` FPS
   counter uses `std::chrono` unconditionally, so the build broke when video
   capture was off (e.g. any non-Apple build before this work).

2. **`src/sdl-test-ui/segMask.cpp`** — guarded the macOS-only CoreML execution
   provider behind `#ifdef __APPLE__` (the `coreml_provider_factory.h` include and
   the `OrtSessionOptionsAppendExecutionProvider_CoreML` call). Linux runs on the
   default CPU provider.

3. **`src/sdl-test-ui/videoCapture_linux.cpp`** (new) — V4L2 camera backend
   implementing the `VideoCapture` contract (mirrors the macOS AVFoundation
   backend): enumerates `/dev/video*` capture nodes, selects by preference-ordered
   case-insensitive name substring (falling back to the first usable device),
   negotiates YUYV/RGB24/BGR24 at 640×480, streams via mmap on a worker thread,
   converts to BGRX, and `Stop()` joins the thread to drain any in-flight callback
   (so tearing down the seg masker mid-inference can't crash). MJPEG-only cameras
   are not yet supported.

4. **`src/sdl-test-ui/CMakeLists.txt`** — default-enable `ENABLE_VIDEO_CAPTURE` on
   Linux, compile `videoCapture_linux.cpp`, define `PROJECTM_VIDEO_CAPTURE_ENABLED`
   whenever a real backend exists, and link `Threads::Threads` for the worker thread.

---

## 5. Configure & build

```shell
cmake -S . -B build \
  -DCMAKE_BUILD_TYPE=Release \
  -DENABLE_SDL_UI=ON \
  -DENABLE_VIDEO_CAPTURE=ON \
  -DENABLE_ONNX_SEG=ON \
  -DCMAKE_PREFIX_PATH="$HOME/.local/onnxruntime"
cmake --build build -j"$(nproc)"
```

Look for these lines during configure to confirm the optional bits are on:

```
-- ONNX seg: using onnxruntime (/home/<you>/.local/onnxruntime/lib/libonnxruntime.so).
--     SDL2 Test UI:                ON
```

> **Cache gotcha:** `ENABLE_VIDEO_CAPTURE` is a cached CMake `option()`. If you ever
> configured the build dir *before* this branch's Linux default existed, the cached
> `OFF` shadows the new default and the V4L2 source is silently skipped. Pass
> `-DENABLE_VIDEO_CAPTURE=ON` explicitly (as above) or delete `build/CMakeCache.txt`.

Plain library-only build (no UI, no ONNX, no camera) just needs:
`cmake -S . -B build -DENABLE_SDL_UI=OFF && cmake --build build`.

---

## 6. Run

The SDL test UI is run from the build tree (it is not installed):

```shell
PROJECTM_VIDEO_MASK=seg \
PROJECTM_PRESET_PATH=presets/tests \
  ./build/src/sdl-test-ui/projectM-Test-UI
```

Capture **auto-starts** in `init()`. Toggle it at runtime with the `v` key.
Expected log lines on success:

```
INFO: Using preset path from $PROJECTM_PRESET_PATH: presets/tests
INFO: [SegMasker] Loaded '.../rvm_mobilenetv3.onnx' (RVM, input 384x384, ratio 1.00).
INFO: ONNX person-seg producing mask in alpha (preset chooses via video_alpha_mode).
```

To actually *see* the webcam matte composited, land on a preset that samples the
video texture (most presets don't). Either point at the `presets/video` directory,
or run a single video preset via `PROJECTM_PRESET_LIST`:

```shell
PROJECTM_VIDEO_MASK=seg \
PROJECTM_PRESET_LIST=<(echo presets/tests/401-compshader-video-alpha.milk) \
  ./build/src/sdl-test-ui/projectM-Test-UI
```

### Relevant environment variables

| Var | Meaning |
|-----|---------|
| `PROJECTM_VIDEO_MASK=seg` | Enable host ONNX person-seg as the alpha-mask producer (`off` = raw video). |
| `PROJECTM_SEG_MODEL` | Path to the `.onnx` model (default `~/.projectM/models/rvm_mobilenetv3.onnx`). |
| `PROJECTM_SEG_MODEL2` / `PROJECTM_SEG_COMBINE` | Optional 2nd model multiplied/gated into the primary matte. |
| `PROJECTM_SEG_QUALITY` | 1/2/3 → 256/384/512 processing size (default 2). |
| `PROJECTM_SEG_SIZE` | Raw processing size override (px). |
| `PROJECTM_VIDEO_DEVICE` | Camera name substring (case-insensitive); else first usable device. |
| `PROJECTM_PRESET_PATH` | Directory of presets to load. |
| `PROJECTM_PRESET_LIST` | File listing presets (one path per line); keeps file order. |

The `justfile` has convenience recipes (`just run` / `just test` / `just video` /
`just preset <file>`), though its default `build` recipe targets the macOS Luxonis +
ONNX setup; on Linux use the explicit `cmake` invocation in §5.

---

## 7. Camera device access

Webcam nodes are `crw-rw----+ root:video`. On a desktop session, `systemd-logind`
grants the active user an ACL (`user:<you>:rw-`) automatically — no need to join the
`video` group. Verify with:

```shell
getfacl /dev/video0 | grep "$USER"     # -> user:<you>:rw-
test -r /dev/video0 && echo READABLE
```

If running headless / over SSH (no active session), add yourself to the group
instead: `sudo usermod -aG video $USER` (re-login required).

---

## 8. Not yet done on Linux

- **Luxonis OAK depth camera** (`ENABLE_LUXONIS`): needs depthai-core built/installed
  standalone (see comments in `src/sdl-test-ui/CMakeLists.txt`). Still a no-op stub.
- **MJPEG** capture format in the V4L2 backend (only YUYV/RGB24/BGR24 today).
- A GPU/NPU ONNX execution provider on Linux (CUDA/TensorRT/DirectML) — currently CPU.
