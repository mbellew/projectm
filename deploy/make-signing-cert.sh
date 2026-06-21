#!/usr/bin/env bash
#
# make-signing-cert.sh — create a stable, self-signed code-signing identity for the appliance.
#
# Without an Apple Developer ID, this gives the deploy binary a *constant* signing identity so the
# camera/microphone TCC grant persists across rebuilds (a self-signed cert need not be trusted for
# TCC — only stable). Run once; deploy/deploy-macos.sh auto-detects and uses it.
#
#   deploy/make-signing-cert.sh ["Identity Name"]      (default: "projectM Appliance")
#
# The cert + private key land in your login keychain. `-A` lets codesign use the key with no
# keychain prompt. The identity is self-signed/untrusted (it shows CSSMERR_TP_NOT_TRUSTED under
# `security find-identity` and is NOT listed by the `-v`/valid filter) — that's expected and fine:
# codesign still signs with it, locally built binaries aren't gatekept, and TCC matches on the
# stable signing identity. So detection below deliberately omits `-v`.
#
set -euo pipefail

NAME="${1:-projectM Appliance}"
KEYCHAIN="${HOME}/Library/Keychains/login.keychain-db"
# System LibreSSL writes Apple-compatible PKCS#12; Homebrew OpenSSL 3.x does not (the keychain
# import fails "MAC verification failed") even with -legacy. Use the system one.
OPENSSL="/usr/bin/openssl"
# `security import` rejects an EMPTY p12 password ("MAC verification failed"); use a throwaway
# non-empty one just for the in-memory handoff. The key itself is stored unprotected in the keychain.
P12PASS="projectm-appliance"

if security find-identity -p codesigning 2>/dev/null | grep -qF "${NAME}"; then
  echo "make-signing-cert: identity '${NAME}' already exists — nothing to do."
  exit 0
fi

[[ -x "${OPENSSL}" ]] || { echo "make-signing-cert: ${OPENSSL} not found." >&2; exit 1; }

tmp="$(mktemp -d)"
trap 'rm -rf "${tmp}"' EXIT

echo "make-signing-cert: generating self-signed code-signing certificate '${NAME}'..."
"${OPENSSL}" req -x509 -newkey rsa:2048 -keyout "${tmp}/key.pem" -out "${tmp}/cert.pem" \
  -days 3650 -nodes -subj "/CN=${NAME}" \
  -addext "basicConstraints=critical,CA:false" \
  -addext "keyUsage=critical,digitalSignature" \
  -addext "extendedKeyUsage=critical,codeSigning" >/dev/null 2>&1

"${OPENSSL}" pkcs12 -export -inkey "${tmp}/key.pem" -in "${tmp}/cert.pem" \
  -out "${tmp}/identity.p12" -passout "pass:${P12PASS}" -name "${NAME}" >/dev/null 2>&1

# -A allows any app (incl. codesign) to use the key without a keychain access prompt.
security import "${tmp}/identity.p12" -P "${P12PASS}" -A -k "${KEYCHAIN}"

echo
echo "Created code-signing identity '${NAME}'."
echo "Build + deploy normally — deploy/deploy-macos.sh signs with it automatically:"
echo "    just deploy-macos && just deploy-to-user brpl"
