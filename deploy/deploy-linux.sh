#!/usr/bin/env bash
#
# deploy-linux.sh — build a self-contained projectM appliance bundle for Linux.  *** TODO ***
#
# Not yet implemented. The intended shape mirrors deploy-macos.sh:
#   bin/projectM-Test-UI
#   lib/*.so*                         bundled shared objects, RPATH rewritten to $ORIGIN/../lib
#   share/projectm/presets/video/
#   share/projectm/palettes/          reference only (palettes are baked into the binary)
#   dotprojectM/{config.inp,textures,models}
#   install.sh
#
# Linux-specific notes for the implementer:
#   - Use `patchelf --set-rpath '$ORIGIN/../lib'` on the binary and `$ORIGIN` on each .so
#     (the analogue of macOS @executable_path / @loader_path); ldd to enumerate deps.
#   - No ad-hoc code-signing step (that's macOS-only).
#   - libGL / libX11 / glibc are the host's — do NOT bundle them; only bundle projectM,
#     playlist, onnxruntime, depthai, libusb, SDL2.
#   - projectM video capture has no Linux backend yet (see appliance/README.md), so the
#     `Video Devices` / seg config is plumbed but inert until one exists.
#
set -euo pipefail
echo "deploy-linux: not yet implemented — see deploy/deploy-macos.sh for the reference design." >&2
exit 1
