#!/usr/bin/env bash
#
# install-to-appliance.sh — install a built deploy bundle into an appliance user's home,
# creating the required directories with the right ownership/permissions and seeding ~/.projectM.
#
# Must run as root (it writes another user's home), so invoke it via sudo:
#
#   sudo deploy/install-to-appliance.sh [USER] [BUNDLE_DIR]
#   just deploy-to-user [user=brpl]
#
#   USER        appliance account to install into          (default: brpl)
#   BUNDLE_DIR  the built bundle                            (default: dist/projectm)
#
# Build the bundle first with `just deploy-macos`. This script is idempotent: re-running it
# refreshes the bundle and re-seeds ~/.projectM (the bundle's install.sh runs with --force).
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

die() { echo "install-to-appliance: error: $*" >&2; exit 1; }
note() { echo "install-to-appliance: $*"; }

[[ "$(id -u)" -eq 0 ]] || die "must run as root — use: sudo deploy/install-to-appliance.sh $*"

USER_NAME="${1:-brpl}"
ARCH="$(uname -m)"
BUNDLE_DIR="${2:-${REPO_ROOT}/dist/projectm}"

[[ -d "${BUNDLE_DIR}" ]] || die "bundle not found: ${BUNDLE_DIR} (run 'just deploy-macos' first)"
[[ -x "${BUNDLE_DIR}/install.sh" ]] || die "bundle is missing install.sh: ${BUNDLE_DIR}"

# Resolve the target user's home + primary group via Directory Services.
HOME_DIR="$(dscl . -read "/Users/${USER_NAME}" NFSHomeDirectory 2>/dev/null | awk '{print $2}')" \
  || die "no such user '${USER_NAME}'"
[[ -n "${HOME_DIR}" ]] || die "could not resolve home for '${USER_NAME}'"
GROUP_NAME="$(id -gn "${USER_NAME}")"
[[ -d "${HOME_DIR}" ]] || die "home directory does not exist: ${HOME_DIR}"

DEST="${HOME_DIR}/$(basename "${BUNDLE_DIR}")"
DOT="${HOME_DIR}/.projectM"

note "user=${USER_NAME} (${GROUP_NAME})  home=${HOME_DIR}"
note "bundle -> ${DEST}"

# 1. Create the required directories, owned by the appliance user.
note "creating directories..."
install -d -o "${USER_NAME}" -g "${GROUP_NAME}" -m 0755 \
  "${DOT}" "${DOT}/textures" "${DOT}/models" "${DOT}/palettes"

# 2. Install the bundle (replace any previous copy), then hand ownership to the appliance user.
note "copying bundle..."
rm -rf "${DEST}"
cp -R "${BUNDLE_DIR}" "${DEST}"
chown -R "${USER_NAME}:${GROUP_NAME}" "${DEST}"

# 3. Seed ~/.projectM by running the bundle's own installer AS the appliance user
#    (so $HOME resolves to their home and the files land owned by them).
note "seeding ~/.projectM (as ${USER_NAME})..."
sudo -u "${USER_NAME}" HOME="${HOME_DIR}" "${DEST}/install.sh" --force
chown -R "${USER_NAME}:${GROUP_NAME}" "${DOT}"

note "done."
echo
echo "Installed to ${DEST}"
echo "Launch (as ${USER_NAME}):"
echo "  PROJECTM_BINARY='${DEST}/bin/projectM-Test-UI' \\"
echo "  PROJECTM_PRESET_PATH='${DEST}/share/projectm/presets/video' \\"
echo "  '${DEST}/bin/projectM-Test-UI' --fullscreen"
