#!/usr/bin/env bash
#
# deploy-linux.sh — assemble a self-contained projectM appliance tree for Linux.
#
# Unlike the macOS bundle (which patchelf/install_name_tool-relocates every dylib), the Linux
# appliance is a plain "system install" tree whose launcher puts <prefix>/lib on LD_LIBRARY_PATH.
# LD_LIBRARY_PATH is searched *before* the binary's RUNPATH, so no patchelf is required: we just
# copy every non-system shared object into lib/ and let the runtime find them by name.
#
# Default target is /opt/projectm (world-readable, shared — avoids the 0750 cross-home perms issue
# and duplicating ~1.3 GB per user). Pass a different OUT_DIR (1st arg) to stage elsewhere, e.g.
# /tmp/projectm for a no-sudo test. Writing to /opt needs root:
#
#   sudo -E BUILD_DIR=cmake-build-linux deploy/deploy-linux.sh              # -> /opt/projectm
#   deploy/deploy-linux.sh /tmp/projectm                                    # -> /tmp (no sudo)
#   just deploy-linux
#
# Source locations (env overrides; defaults match LINUX_SETUP.md / the justfile):
#   BUILD_DIR          build tree                 (default: <repo>/cmake-build-linux)
#   ONNX_PREFIX        onnxruntime prefix         (default: $HOME/.local/onnxruntime)
#   CUDA_RUNTIME_DIR   CUDA/cuDNN runtime libs    (default: $HOME/.local/cuda-runtime/lib)
#   MODELS_DIR         *.onnx seg/depth models    (default: $HOME/.projectM/models)
#   TEXTURES_DIR       image-sampler textures     (default: $HOME/.projectM/textures)
#   PRESETS_DIR        .milk presets to ship      (default: <repo>/presets/video)
#   INCLUDE_MODELS     0 to skip copying models   (default: 1)
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

die()  { echo "deploy-linux: error: $*" >&2; exit 1; }
note() { echo "deploy-linux: $*"; }

[[ "$(uname -s)" == "Linux" ]] || die "this script is for Linux; use deploy-macos.sh on macOS."

OUT_DIR="${1:-/opt/projectm}"
BUILD_DIR="${BUILD_DIR:-${REPO_ROOT}/cmake-build-linux}"
ONNX_PREFIX="${ONNX_PREFIX:-${HOME}/.local/onnxruntime}"
CUDA_RUNTIME_DIR="${CUDA_RUNTIME_DIR:-${HOME}/.local/cuda-runtime/lib}"
MODELS_DIR="${MODELS_DIR:-${HOME}/.projectM/models}"
TEXTURES_DIR="${TEXTURES_DIR:-${HOME}/.projectM/textures}"
PRESETS_DIR="${PRESETS_DIR:-${REPO_ROOT}/presets/video}"
INCLUDE_MODELS="${INCLUDE_MODELS:-1}"

BIN_SRC="${BUILD_DIR}/src/sdl-test-ui/projectM-Test-UI"
[[ -x "${BIN_SRC}" ]] || die "binary not found: ${BIN_SRC} (run 'just build' first)"
[[ -d "${ONNX_PREFIX}/lib" ]] || die "onnxruntime libs not found: ${ONNX_PREFIX}/lib"
[[ -d "${CUDA_RUNTIME_DIR}" ]] || note "warning: CUDA runtime dir not found: ${CUDA_RUNTIME_DIR} (GPU seg will fall back to CPU)"

note "target   : ${OUT_DIR}"
note "binary   : ${BIN_SRC}"
note "onnx     : ${ONNX_PREFIX}/lib"
note "cuda     : ${CUDA_RUNTIME_DIR}"
note "models   : ${MODELS_DIR} (include=${INCLUDE_MODELS})"
note "textures : ${TEXTURES_DIR}"
note "presets  : ${PRESETS_DIR}"

# Fail early with a friendly message if the target needs root.
parent="$(dirname "${OUT_DIR}")"
if [[ ! -e "${OUT_DIR}" && ! -w "${parent}" ]] || [[ -e "${OUT_DIR}" && ! -w "${OUT_DIR}" ]]; then
  die "cannot write ${OUT_DIR} — re-run with sudo (use 'sudo -E' so the source paths/env survive)."
fi

mkdir -p "${OUT_DIR}/bin" "${OUT_DIR}/lib" \
         "${OUT_DIR}/share/projectm/presets/video" "${OUT_DIR}/models" "${OUT_DIR}/textures"

# 1. Binary.
note "copying binary..."
install -m 0755 "${BIN_SRC}" "${OUT_DIR}/bin/projectM-Test-UI"

# 2. Shared objects (cp -a preserves the .so -> .so.N -> .so.N.M symlink chains). SDL2, libGL,
#    libX11 and glibc are the host's and intentionally NOT bundled.
note "copying projectM libs..."
cp -a "${BUILD_DIR}/src/libprojectM/"libprojectM-4.so* "${OUT_DIR}/lib/" 2>/dev/null || \
  die "libprojectM-4.so* not found under ${BUILD_DIR}/src/libprojectM"
cp -a "${BUILD_DIR}/src/playlist/"libprojectM-4-playlist.so* "${OUT_DIR}/lib/" 2>/dev/null || \
  die "libprojectM-4-playlist.so* not found under ${BUILD_DIR}/src/playlist"

note "copying onnxruntime libs..."
cp -a "${ONNX_PREFIX}/lib/"*.so* "${OUT_DIR}/lib/"

if [[ -d "${CUDA_RUNTIME_DIR}" ]]; then
  note "copying CUDA runtime libs (~1 GB, this takes a moment)..."
  cp -a "${CUDA_RUNTIME_DIR}/"*.so* "${OUT_DIR}/lib/"
fi

# 3. Presets.
note "copying presets..."
if [[ -d "${PRESETS_DIR}" ]]; then
  cp -a "${PRESETS_DIR}/." "${OUT_DIR}/share/projectm/presets/video/"
else
  note "warning: presets dir ${PRESETS_DIR} not found; shipping empty preset dir."
fi

# 4. Models (RVM + optional depth/u2net/yolo). Referenced from config.inp by absolute path.
if [[ "${INCLUDE_MODELS}" == "1" && -d "${MODELS_DIR}" ]]; then
  note "copying models..."
  cp -a "${MODELS_DIR}/"*.onnx "${OUT_DIR}/models/" 2>/dev/null || \
    note "warning: no *.onnx in ${MODELS_DIR}"
else
  note "skipping models (INCLUDE_MODELS=${INCLUDE_MODELS})."
fi

# 4b. Textures for image samplers (config.inp's "Texture Path" points here).
if [[ -d "${TEXTURES_DIR}" ]] && compgen -G "${TEXTURES_DIR}/*" >/dev/null; then
  note "copying textures..."
  cp -a "${TEXTURES_DIR}/." "${OUT_DIR}/textures/"
else
  note "no textures in ${TEXTURES_DIR} (skipping)."
fi

# 5. Launcher wrapper (derives its prefix from its own location at runtime).
note "installing run-projectm.sh..."
install -m 0755 "${REPO_ROOT}/appliance/launch/run-projectm-linux.sh" \
  "${OUT_DIR}/run-projectm.sh"

# 6. Make the tree group/other-readable so the appliance user can run it.
chmod -R a+rX "${OUT_DIR}"

note "done."
echo
echo "Installed tree at ${OUT_DIR}"
echo "Test it (from any account that can read ${OUT_DIR}):"
echo "  ${OUT_DIR}/run-projectm.sh"
du -sh "${OUT_DIR}" 2>/dev/null || true
