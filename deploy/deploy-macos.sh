#!/usr/bin/env bash
#
# deploy-macos.sh — build a self-contained, relocatable projectM appliance bundle for macOS.
#
# The bundle has no dependency on the build tree, Homebrew, or ~/.local prefixes: every
# non-system dylib is copied into lib/ and the binary's install-names / rpaths are rewritten to
# @executable_path/../lib (and the Mach-O files are ad-hoc re-signed, required on Apple Silicon).
#
# Output layout (OUT_DIR/<name>/):
#   bin/projectM-Test-UI              the test UI binary
#   lib/*.dylib                       all bundled, relocated dynamic libraries
#   share/projectm/presets/video/     the video presets (*.milk)
#   share/projectm/palettes/          curated palette PNGs (REFERENCE ONLY — baked into the binary)
#   dotprojectM/config.inp            seed appliance config (from appliance/config/config.inp.example)
#   dotprojectM/textures/             image-sampler textures  (from ~/.projectM/textures)
#   dotprojectM/models/               ONNX seg models         (from ~/.projectM/models)
#   install.sh                        seeds ~/.projectM on the target and prints the launch env
#   README.md
#
# A .tar.gz of the bundle is also produced next to it.
#
# Usage:
#   deploy/deploy-macos.sh [OUT_DIR]
#
# Environment overrides:
#   BUILD_DIR        build tree (default: cmake-build-luxonis)
#   ONNX_PREFIX      onnxruntime install prefix (default: ~/.local/onnxruntime)
#   DEPTHAI_PREFIX   depthai-core install prefix (default: ~/.local/depthai-core)
#   DOTPROJECTM_SRC  source for textures/models (default: ~/.projectM)
#   INCLUDE_MODELS   1 to bundle ~/.projectM/models (default: 1; ~215MB)
#   INCLUDE_TEXTURES 1 to bundle ~/.projectM/textures (default: 1)
#   NO_TARBALL       1 to skip the .tar.gz step
#
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${REPO_ROOT}"

BUILD_DIR="${BUILD_DIR:-cmake-build-luxonis}"
ONNX_PREFIX="${ONNX_PREFIX:-${HOME}/.local/onnxruntime}"
DEPTHAI_PREFIX="${DEPTHAI_PREFIX:-${HOME}/.local/depthai-core}"
DOTPROJECTM_SRC="${DOTPROJECTM_SRC:-${HOME}/.projectM}"
INCLUDE_MODELS="${INCLUDE_MODELS:-1}"
INCLUDE_TEXTURES="${INCLUDE_TEXTURES:-1}"

ARCH="$(uname -m)"
# Short, easy-to-type bundle directory; the tarball keeps the arch so archives don't collide.
BUNDLE_NAME="${BUNDLE_NAME:-projectm}"
OUT_DIR="${1:-${REPO_ROOT}/dist}"
STAGE="${OUT_DIR}/${BUNDLE_NAME}"
TARBALL="${OUT_DIR}/${BUNDLE_NAME}-macos-${ARCH}.tar.gz"

BINARY_SRC="${BUILD_DIR}/src/sdl-test-ui/projectM-Test-UI"

# @rpath / @loader_path basename search paths (highest priority first).
SEARCH_PATHS=(
  "${BUILD_DIR}/src/libprojectM"
  "${BUILD_DIR}/src/playlist"
  "${ONNX_PREFIX}/lib"
  "${DEPTHAI_PREFIX}/lib"
  "${DEPTHAI_PREFIX}/lib/cmake/depthai/dependencies/lib"
  "/opt/homebrew/opt/sdl2/lib"
  "/opt/homebrew/lib"
)

die() { echo "deploy-macos: error: $*" >&2; exit 1; }
note() { echo "deploy-macos: $*"; }

[[ -x "${BINARY_SRC}" ]] || die "binary not found: ${BINARY_SRC} (run 'just build' first, or set BUILD_DIR)"

note "staging at ${STAGE}"
rm -rf "${STAGE}"
mkdir -p "${STAGE}/bin" "${STAGE}/lib" "${STAGE}/share/projectm" "${STAGE}/dotprojectM"

# ---------------------------------------------------------------------------
# 1. Binary + recursive dylib bundling
# ---------------------------------------------------------------------------
cp "${BINARY_SRC}" "${STAGE}/bin/projectM-Test-UI"
chmod u+w "${STAGE}/bin/projectM-Test-UI"

# Resolve an otool dependency string to a real source file via SEARCH_PATHS (by basename).
resolve_dep() {
  local dep="$1" base
  base="$(basename "${dep}")"
  case "${dep}" in
    @rpath/*|@loader_path/*|@executable_path/*)
      local p
      for p in "${SEARCH_PATHS[@]}"; do
        [[ -e "${p}/${base}" ]] && { echo "${p}/${base}"; return 0; }
      done
      return 1
      ;;
    /*)
      [[ -e "${dep}" ]] && { echo "${dep}"; return 0; }
      return 1
      ;;
    *)
      return 1
      ;;
  esac
}

# Non-system, relocatable dependency? (skip OS libs and the file's own id line)
is_bundle_dep() {
  case "$1" in
    /usr/lib/*|/System/*) return 1 ;;
    @rpath/*|@loader_path/*|@executable_path/*|/*) return 0 ;;
    *) return 1 ;;
  esac
}

# otool -L without the leading "self" line.
list_deps() { otool -L "$1" | tail -n +2 | awk '{print $1}'; }

declare -A STAGED=()   # basename -> 1 once copied into lib/
QUEUE=()

# Rewrite parent's references to any bundled dep -> @rpath/<base>, and enqueue new deps.
process_macho() {
  local file="$1" dep base src
  while IFS= read -r dep; do
    [[ -z "${dep}" ]] && continue
    is_bundle_dep "${dep}" || continue
    base="$(basename "${dep}")"
    # Skip a dylib's own id line (basename == file's basename and it's an @rpath self-ref).
    if [[ "${base}" == "$(basename "${file}")" && "${dep}" == @rpath/* ]]; then
      continue
    fi
    if [[ -z "${STAGED[${base}]:-}" ]]; then
      src="$(resolve_dep "${dep}")" || die "cannot resolve dependency '${dep}' (needed by ${file})"
      src="$(/usr/bin/python3 -c 'import os,sys;print(os.path.realpath(sys.argv[1]))' "${src}")"
      cp "${src}" "${STAGE}/lib/${base}"
      chmod u+w "${STAGE}/lib/${base}"
      install_name_tool -id "@rpath/${base}" "${STAGE}/lib/${base}" 2>/dev/null || true
      install_name_tool -add_rpath "@loader_path" "${STAGE}/lib/${base}" 2>/dev/null || true
      STAGED[${base}]=1
      QUEUE+=("${STAGE}/lib/${base}")
      note "  bundled ${base}  <- ${src}"
    fi
    install_name_tool -change "${dep}" "@rpath/${base}" "${file}" 2>/dev/null || true
  done < <(list_deps "${file}")
}

note "bundling dylibs..."
# Strip the build-tree / Homebrew / ~/.local rpaths so resolution can only use the bundled lib/.
# (Otherwise those absolute rpaths win the search order and the bundle silently loads non-bundled
# libs on the build machine, hiding relocation bugs.)
while IFS= read -r rp; do
  case "${rp}" in
    @executable_path/*|@loader_path/*) ;;  # keep relocatable rpaths
    *) install_name_tool -delete_rpath "${rp}" "${STAGE}/bin/projectM-Test-UI" 2>/dev/null || true ;;
  esac
done < <(otool -l "${STAGE}/bin/projectM-Test-UI" | awk '/LC_RPATH/{f=1} f&&/path /{print $2; f=0}')
install_name_tool -add_rpath "@executable_path/../lib" "${STAGE}/bin/projectM-Test-UI" 2>/dev/null || true
process_macho "${STAGE}/bin/projectM-Test-UI"
# Drain the queue (recursively process freshly-copied libraries).
while [[ ${#QUEUE[@]} -gt 0 ]]; do
  current="${QUEUE[0]}"
  QUEUE=("${QUEUE[@]:1}")
  process_macho "${current}"
done

# Re-sign every Mach-O we touched (required on Apple Silicon after install_name_tool).
# Prefer a stable self-signed identity (so the camera/mic TCC grant persists across rebuilds);
# fall back to ad-hoc. Override with SIGN_IDENTITY=... ('-' forces ad-hoc).
# Note: no -v — a self-signed identity is untrusted (CSSMERR_TP_NOT_TRUSTED) and absent from the
# "valid" list, but codesign still signs with it and TCC matches on it.
if [[ -z "${SIGN_IDENTITY:-}" ]]; then
  if security find-identity -p codesigning 2>/dev/null | grep -qF "projectM Appliance"; then
    SIGN_IDENTITY="projectM Appliance"
  else
    SIGN_IDENTITY="-"
  fi
fi
if [[ "${SIGN_IDENTITY}" == "-" ]]; then
  note "re-signing (ad-hoc — TCC grants won't persist across rebuilds; see deploy/make-signing-cert.sh)..."
else
  note "re-signing (identity: ${SIGN_IDENTITY})..."
fi
# Sign libs first (inner), the executable last.
for f in "${STAGE}"/lib/*.dylib "${STAGE}/bin/projectM-Test-UI"; do
  if ! codesign --force --sign "${SIGN_IDENTITY}" "${f}" 2>/tmp/pm_codesign.err; then
    cat /tmp/pm_codesign.err >&2
    [[ "${SIGN_IDENTITY}" == "-" ]] || die "codesign failed with identity '${SIGN_IDENTITY}'. Run deploy/make-signing-cert.sh, or set SIGN_IDENTITY=- for ad-hoc."
  fi
done
rm -f /tmp/pm_codesign.err

# ---------------------------------------------------------------------------
# 2. Presets + palettes (share/)
# ---------------------------------------------------------------------------
note "copying presets + palettes..."
mkdir -p "${STAGE}/share/projectm/presets/video"
cp presets/video/*.milk "${STAGE}/share/projectm/presets/video/" 2>/dev/null || die "no presets in presets/video"
if compgen -G "palettes/*.png" >/dev/null; then
  mkdir -p "${STAGE}/share/projectm/palettes"
  cp palettes/*.png "${STAGE}/share/projectm/palettes/"
fi

# ---------------------------------------------------------------------------
# 3. ~/.projectM seed (config.inp, textures, models)
# ---------------------------------------------------------------------------
note "staging ~/.projectM seed..."
cp appliance/config/config.inp.example "${STAGE}/dotprojectM/config.inp"
if [[ "${INCLUDE_TEXTURES}" == "1" && -d "${DOTPROJECTM_SRC}/textures" ]]; then
  mkdir -p "${STAGE}/dotprojectM/textures"
  cp -R "${DOTPROJECTM_SRC}/textures/." "${STAGE}/dotprojectM/textures/"
  rm -f "${STAGE}/dotprojectM/textures/.DS_Store"
fi
if [[ "${INCLUDE_MODELS}" == "1" && -d "${DOTPROJECTM_SRC}/models" ]]; then
  mkdir -p "${STAGE}/dotprojectM/models"
  # Models only (skip the regenerable coreml_cache and any stray dotfiles).
  find "${DOTPROJECTM_SRC}/models" -maxdepth 1 -type f -name '*.onnx' -exec cp {} "${STAGE}/dotprojectM/models/" \;
fi

# ---------------------------------------------------------------------------
# 4. install.sh + README inside the bundle
# ---------------------------------------------------------------------------
cat > "${STAGE}/install.sh" <<'INSTALL'
#!/usr/bin/env bash
#
# install.sh — seed ~/.projectM from this bundle and print the launch environment.
# Run this once on the appliance (as the appliance user). The bundle itself is the install root;
# nothing is copied out of bin/ lib/ share/ — only the ~/.projectM seed is installed.
#
#   ./install.sh           # seed ~/.projectM (won't overwrite an existing config.inp)
#   ./install.sh --force    # also overwrite an existing config.inp
#
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FORCE=0; [[ "${1:-}" == "--force" ]] && FORCE=1
DST="${HOME}/.projectM"
mkdir -p "${DST}"

if [[ -f "${DST}/config.inp" && "${FORCE}" != "1" ]]; then
  echo "install: keeping existing ${DST}/config.inp (pass --force to overwrite)"
else
  cp "${HERE}/dotprojectM/config.inp" "${DST}/config.inp"
  echo "install: wrote ${DST}/config.inp"
fi
for d in textures models; do
  if [[ -d "${HERE}/dotprojectM/${d}" ]]; then
    mkdir -p "${DST}/${d}"
    cp -R "${HERE}/dotprojectM/${d}/." "${DST}/${d}/"
    echo "install: populated ${DST}/${d}"
  fi
done

cat <<ENV

Done. Launch the appliance with:

  export PROJECTM_BINARY="${HERE}/bin/projectM-Test-UI"
  export PROJECTM_PRESET_PATH="${HERE}/share/projectm/presets/video"
  "${HERE}/bin/projectM-Test-UI" --fullscreen

(or point the appliance LaunchAgent's PROJECTM_BINARY / PROJECTM_PRESET_PATH at those paths.)
ENV
INSTALL
chmod +x "${STAGE}/install.sh"

cat > "${STAGE}/README.md" <<README
# projectM appliance bundle (macOS ${ARCH})

Self-contained, relocatable. Move this folder anywhere and run it.

## Install

\`\`\`bash
./install.sh          # seeds ~/.projectM (config.inp, textures, models)
\`\`\`

## Run

\`\`\`bash
export PROJECTM_BINARY="\$PWD/bin/projectM-Test-UI"
export PROJECTM_PRESET_PATH="\$PWD/share/projectm/presets/video"
./bin/projectM-Test-UI --fullscreen
\`\`\`

## Layout

- \`bin/\` — the test UI binary (rpath → \`@executable_path/../lib\`)
- \`lib/\` — all bundled dylibs (projectM, playlist, onnxruntime, depthai, libusb, SDL2)
- \`share/projectm/presets/video/\` — video presets
- \`share/projectm/palettes/\` — palette PNGs, **reference only** (palettes are baked into the binary)
- \`dotprojectM/\` — seed for \`~/.projectM\` (config.inp, textures, models)

First launch will prompt for camera/microphone permission (needed for video presets / seg masking).
README

# ---------------------------------------------------------------------------
# 5. Tarball
# ---------------------------------------------------------------------------
if [[ "${NO_TARBALL:-0}" != "1" ]]; then
  note "creating tarball..."
  tar -C "${OUT_DIR}" -czf "${TARBALL}" "${BUNDLE_NAME}"
  note "wrote ${TARBALL}"
fi

note "done. bundle: ${STAGE}"
du -sh "${STAGE}" 2>/dev/null || true
