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
`libonnxruntime.so` under this prefix via `CMAKE_PREFIX_PATH` (see §5).

This CPU package is enough to **build** everything and run seg on the CPU. RVM on
the CPU only produces a few matte frames per second, though, so for a responsive
mask use the NVIDIA GPU build instead — see **§9. GPU acceleration**. The build
itself is identical either way (the GPU providers load at runtime); only the
installed ONNX Runtime package and the runtime library path differ.

### Person-seg model → `~/.projectM/models/rvm_mobilenetv3.onnx`

Robust Video Matting (mobilenetv3), the app's default model path:

```shell
mkdir -p ~/.projectM/models
curl -fsSL -o ~/.projectM/models/rvm_mobilenetv3.onnx \
  https://github.com/PeterL1n/RobustVideoMatting/releases/download/v1.0.0/rvm_mobilenetv3_fp32.onnx
```

`segMask.cpp` auto-detects RVM by its `downsample_ratio` input. Other supported
families: MODNet, U²-Net (`*u2net*` filename), YOLOv8/v11-seg (`*yolo*` filename).

### Optional depth model → `~/.projectM/models/depth_anything_v2_vits.onnx`

To drop **background people** (spectators / passers-by) from the matte while keeping
everyone up front, add a monocular depth model. Depth Anything V2 Small (fp32, ~99 MB):

```shell
curl -fsSL -o ~/.projectM/models/depth_anything_v2_vits.onnx \
  https://huggingface.co/onnx-community/depth-anything-v2-small/resolve/main/onnx/model.onnx
```

Enable it by pointing `PROJECTM_SEG_DEPTH_MODEL` (or the `Video Seg Depth Model` config
key) at the file. Each frame the matte is split into connected components, each one's
median relative depth is measured, and components far behind the nearest are faded out.
Tune with `PROJECTM_SEG_DEPTH_BAND` (see the env table below). Base (`*vitb*`) is a
drop-in swap for sharper depth at ~3-4× the cost.
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
   negotiates YUYV/RGB24/BGR24/MJPEG at 640×480, streams via mmap on a worker
   thread, converts to BGRX (MJPEG frames are decoded with the vendored stb_image),
   and `Stop()` joins the thread to drain any in-flight callback (so tearing down
   the seg masker mid-inference can't crash). YUYV is preferred when offered.

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
| `PROJECTM_SEG_CUDA` | `1` (default) tries the NVIDIA CUDA EP, falls back to CPU; `0` forces CPU. See §9. |
| `PROJECTM_SEG_CUDA_DEVICE` | CUDA device index (default 0). |
| `PROJECTM_SEG_QUALITY` | 1/2/3 → 256/384/512 processing size (default 2). |
| `PROJECTM_SEG_SIZE` | Raw processing size override (px). |
| `PROJECTM_SEG_DEPTH_MODEL` | Optional monocular depth model (e.g. `~/.projectM/models/depth_anything_v2_vits.onnx`). When set, background people (spectators/passers-by) are dropped from the matte by relative depth. |
| `PROJECTM_SEG_DEPTH_BAND` | Keep band (0..1, default 0.20): how far behind the nearest person still counts as "front". Larger keeps more; `0` keeps only the closest. |
| `PROJECTM_SEG_DEPTH_SIZE` | Depth processing long-side px (default 392, snapped to a multiple of 14). |
| `PROJECTM_SEG_DEPTH_INVERT` | Set `1` if the depth model outputs larger = farther (Depth Anything is larger = closer, the default). |
| `PROJECTM_SEG_DEPTH_DEBUG` | `1` logs per-component closeness + keep weight every ~60 frames, for tuning `BAND`. |
| `PROJECTM_VIDEO_DEVICE` | Camera name substring (case-insensitive); else first usable device. |
| `PROJECTM_PRESET_PATH` | Directory of presets to load. |
| `PROJECTM_PRESET_LIST` | File listing presets (one path per line); keeps file order. |

The `justfile` is OS-aware: on Linux `just build` configures the V4L2 + ONNX build
into `cmake-build-linux/` (the §5 invocation), and `just run` / `just test` /
`just video` / `just preset <file>` run the test UI against the respective preset
dirs. (On macOS the same recipes target the Luxonis + ONNX build in
`cmake-build-luxonis/`.) `just build` does not set `PROJECTM_VIDEO_MASK`; export it
yourself to enable seg, e.g. `PROJECTM_VIDEO_MASK=seg just test`.

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
- **TensorRT** EP (the ORT GPU package ships `libonnxruntime_providers_tensorrt.so`,
  but it needs a TensorRT install and is not wired up). CUDA EP is supported — §9.

---

## 9. GPU acceleration (NVIDIA CUDA, optional)

RVM person-seg on the CPU produces the matte at only a few frames per second, so
the masked video lags the camera even though the visualizer renders at 60 FPS.
Running the seg model on an NVIDIA GPU via the ONNX Runtime **CUDA execution
provider** lets the matte keep up.

The code attempts the CUDA EP automatically on non-Apple platforms
(`segMask.cpp`) and **falls back to CPU** if anything is missing — so this whole
section is optional. `PROJECTM_SEG_CUDA=0` forces CPU.

> **Heads-up: this is a heavy, fiddly add-on** (~1.5 GB of NVIDIA libraries, exact
> version matching across driver/CUDA/cuDNN/ORT). The build does not change — only
> the installed ONNX Runtime package and a runtime `LD_LIBRARY_PATH` differ. If you
> don't need a real-time matte, skip it and stay on the CPU package from §3.

### Requirements

- An NVIDIA GPU + driver new enough for **CUDA 13** (`nvidia-smi` shows the
  capable "CUDA Version"). Blackwell cards (e.g. RTX 50-series, sm_120) **require**
  CUDA 13 — the ORT CUDA 12 build won't have native kernels for them.
- No CUDA toolkit and no `sudo` needed: we fetch only the prebuilt runtime `.so`
  files from NVIDIA's **redistributable tarballs** (not pip, not the full toolkit).

### Step 1 — GPU ONNX Runtime build (replaces the CPU one at the same prefix)

```shell
ver=1.27.0
cd /tmp
curl -fsSLO https://github.com/microsoft/onnxruntime/releases/download/v${ver}/onnxruntime-linux-x64-gpu_cuda13-${ver}.tgz
tar xzf onnxruntime-linux-x64-gpu_cuda13-${ver}.tgz
mv ~/.local/onnxruntime ~/.local/onnxruntime-cpu-bak    # keep the CPU build around
mkdir -p ~/.local/onnxruntime
cp -r onnxruntime-linux-x64-gpu_cuda13-${ver}/include ~/.local/onnxruntime/
cp -r onnxruntime-linux-x64-gpu_cuda13-${ver}/lib     ~/.local/onnxruntime/
```

The GPU package is a superset of the CPU one (same headers + `libonnxruntime.so`,
plus `libonnxruntime_providers_cuda.so`). The project's CMake config (`§5`) is
unchanged — `libonnxruntime.so`'s soname is identical, so **no rebuild is needed**.

### Step 2 — CUDA 13 + cuDNN 9 runtime libraries (NVIDIA redist tarballs)

`libonnxruntime_providers_cuda.so` dlopens these at runtime:
`libcudart.so.13`, `libcublas.so.13`, `libcublasLt.so.13`, `libcufft.so.12`,
`libcurand.so.10`, `libnvrtc.so.13`, `libcudnn.so.9` (`libcuda.so.1` comes from the
driver). Fetch just those components and collect their `.so` files into one dir:

```shell
mkdir -p /tmp/cudaredist && cd /tmp/cudaredist
CUDA=https://developer.download.nvidia.com/compute/cuda/redist
CUDNN=https://developer.download.nvidia.com/compute/cudnn/redist
curl -fsSLO $CUDA/cuda_cudart/linux-x86_64/cuda_cudart-linux-x86_64-13.0.96-archive.tar.xz
curl -fsSLO $CUDA/cuda_nvrtc/linux-x86_64/cuda_nvrtc-linux-x86_64-13.0.88-archive.tar.xz
curl -fsSLO $CUDA/libcublas/linux-x86_64/libcublas-linux-x86_64-13.1.0.3-archive.tar.xz
curl -fsSLO $CUDA/libcufft/linux-x86_64/libcufft-linux-x86_64-12.0.0.61-archive.tar.xz
curl -fsSLO $CUDA/libcurand/linux-x86_64/libcurand-linux-x86_64-10.4.0.35-archive.tar.xz
curl -fsSLO $CUDNN/cudnn/linux-x86_64/cudnn-linux-x86_64-9.14.0.64_cuda13-archive.tar.xz
for f in *.tar.xz; do tar xf "$f"; done
mkdir -p ~/.local/cuda-runtime/lib
# NOTE the `-not -path '*/stubs/*'`: the libcublas tarball ships a tiny link-time
# *stub* libcublas.so / libcublasLt.so under lib/stubs/ alongside the real (50 MB /
# 540 MB) libraries. Without the exclusion the stub flattens into the dir and wins
# the soname, so cublasCreate() returns error 50 at session init and the ORT CUDA
# provider hard-crashes in its own error formatter (not a catchable fallback).
find . -path '*/lib/*' -name '*.so*' -not -path '*/stubs/*' \
    -exec cp -P {} ~/.local/cuda-runtime/lib/ \;
```

Sanity-check that the real (not stub) cuBLAS landed — the soname should resolve to a
multi-MB file and carry no "stub version" string:

```shell
stat -L -c '%s  %n' ~/.local/cuda-runtime/lib/libcublas.so.13   # want ~50 MB, not ~75 KB
strings -a ~/.local/cuda-runtime/lib/libcublas.so.13 | grep -i 'stub version' \
    && echo 'STUB — re-copy excluding */stubs/*' || echo 'real cuBLAS OK'
```

> Versions are from the CUDA `13.0.2` and cuDNN `9.14.0` redist manifests
> (`developer.download.nvidia.com/compute/{cuda,cudnn}/redist/redistrib_<ver>.json`).
> Pick newer point releases the same way if these age out — any CUDA 13.x / cuDNN 9.x
> matching the ORT cuda13 build works.

### Step 3 — Run with the CUDA libraries on the loader path

```shell
LD_LIBRARY_PATH="$HOME/.local/cuda-runtime/lib:$LD_LIBRARY_PATH" \
PROJECTM_VIDEO_MASK=seg \
PROJECTM_PRESET_PATH=presets/tests \
  ./build/src/sdl-test-ui/projectM-Test-UI
```

`libonnxruntime.so` finds its sibling CUDA provider via its own rpath; the provider
finds the CUDA/cuDNN libs via `LD_LIBRARY_PATH`. Success log line:

```
INFO: [SegMasker] Using CUDA execution provider (device 0).
```

If instead you see `CUDA EP unavailable (...); using CPU`, the provider or a CUDA
library failed to load — check `LD_LIBRARY_PATH` and that every `.so` above is in
`~/.local/cuda-runtime/lib`. To revert to the CPU build:
`rm -rf ~/.local/onnxruntime && mv ~/.local/onnxruntime-cpu-bak ~/.local/onnxruntime`.

If instead the process **segfaults right after** the "Using CUDA execution provider"
line (during session init, in `__strlen` inside `libonnxruntime_providers_cuda.so`),
a CUDA library loaded but a call into it failed and ORT crashed in its own error
formatter rather than throwing. The usual cause is a **stub cuBLAS** — see the
sanity-check in Step 2; re-copy excluding `*/stubs/*`.
