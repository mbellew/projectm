#!/usr/bin/env bash
#
# run-projectm.sh — launch projectM as an appliance.
#
# Invoked by the LaunchAgent (and usable by hand for debugging). Configuration comes from
# environment variables, which the LaunchAgent sets from the Ansible group_vars:
#
#   PROJECTM_BINARY        absolute path to projectM-Test-UI (or pass as $1)
#   PROJECTM_PRESET_PATH   preset directory          (optional)
#   PROJECTM_PRESET_LIST   favorites list file       (optional; takes precedence if set)
#
# Fullscreen and audio/video source preferences are read from ~/.projectM/config.inp; we also
# pass --fullscreen so the appliance is fullscreen even if the config file is missing.
#
# caffeinate keeps the display awake while projectM runs.
#
set -euo pipefail

BINARY="${PROJECTM_BINARY:-${1:-}}"
if [[ -z "${BINARY}" || ! -x "${BINARY}" ]]; then
  echo "run-projectm.sh: projectM binary not found or not executable: '${BINARY}'" >&2
  echo "  set \$PROJECTM_BINARY or pass the path as the first argument." >&2
  exit 1
fi

exec /usr/bin/caffeinate -dimsu "${BINARY}" --fullscreen
