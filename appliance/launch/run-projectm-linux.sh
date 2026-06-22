#!/usr/bin/env bash
#
# run-projectm-linux.sh — launch projectM as a Linux appliance.
#
# Installed alongside the system bundle (e.g. /opt/projectm/run-projectm.sh) and invoked by the
# per-user XDG autostart entry (and usable by hand for debugging). Unlike macOS, the Linux build
# needs its bundled shared objects (projectM, onnxruntime, and the ~1 GB CUDA runtime) on
# LD_LIBRARY_PATH, so this wrapper points there before exec'ing the binary.
#
# The bundle prefix is derived from this script's own location, so the same script works whether
# it lives in /opt/projectm or a throwaway test dir. Layout it expects:
#   <prefix>/bin/projectM-Test-UI
#   <prefix>/lib/*.so*            (projectM + onnxruntime + CUDA runtime)
#   <prefix>/models/*.onnx        (referenced from config.inp by absolute path)
#   <prefix>/share/projectm/presets/video/
#
# Environment overrides (the autostart entry / config.inp normally supply these):
#   PROJECTM_BINARY        absolute path to the binary   (default <prefix>/bin/projectM-Test-UI)
#   PROJECTM_PRESET_PATH   preset directory              (default <prefix>/share/projectm/presets/video)
#   PROJECTM_PRESET_LIST   favorites list file           (optional; takes precedence if set)
#
# Restart policy mirrors the macOS LaunchAgent: relaunch only on a *crash* (non-zero exit), so a
# deliberate quit (clean exit 0) stays quit. Set PROJECTM_RESTART=0 to disable the relaunch loop.
set -uo pipefail

PREFIX="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

export LD_LIBRARY_PATH="${PREFIX}/lib${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}"

BINARY="${PROJECTM_BINARY:-${PREFIX}/bin/projectM-Test-UI}"
if [[ ! -x "${BINARY}" ]]; then
  echo "run-projectm-linux.sh: binary not found or not executable: '${BINARY}'" >&2
  exit 1
fi

# Preset source is normally set by config.inp's "Preset Path" (the config role points it at
# <prefix>/share/projectm/presets/video). We deliberately do NOT export PROJECTM_PRESET_PATH here,
# because the env var *overrides* config.inp — leaving it unset lets config.inp stay authoritative.
# A caller may still pass PROJECTM_PRESET_PATH / PROJECTM_PRESET_LIST explicitly to override.

run() { "${BINARY}" --fullscreen "$@"; }

if [[ "${PROJECTM_RESTART:-1}" == "0" ]]; then
  exec "${BINARY}" --fullscreen "$@"
fi

# Relaunch on crash only. A clean exit (0) breaks the loop so a deliberate quit stays quit.
while true; do
  if run "$@"; then
    break
  fi
  echo "run-projectm-linux.sh: projectM exited non-zero ($?); restarting in 5s..." >&2
  sleep 5
done
