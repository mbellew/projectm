#!/usr/bin/env python3
"""Write /etc/kcpassword for macOS automatic login.

macOS stores the auto-login password XOR-obfuscated (NOT encrypted) with a fixed 11-byte key.
The password is read from the KC_PASSWORD environment variable so it never appears in argv/ps.

This is intentionally low-security: see appliance/README.md. Use a dedicated throwaway account.
"""
import os
import sys

# The fixed cipher key macOS uses for /etc/kcpassword.
KEY = bytes([0x7D, 0x89, 0x52, 0x23, 0xD2, 0xBC, 0xDD, 0xEA, 0xA3, 0xB9, 0x1F])
KCPASSWORD_PATH = "/etc/kcpassword"


def obfuscate(password: str) -> bytes:
    plain = bytearray(password.encode("utf-8"))
    # Pad with NUL bytes to the next strict multiple of 12 (always adds 1..12 bytes); some
    # macOS versions reject a file whose length equals the raw password length.
    target = ((len(plain) // 12) + 1) * 12
    plain.extend(b"\x00" * (target - len(plain)))
    return bytes(b ^ KEY[i % len(KEY)] for i, b in enumerate(plain))


def main() -> int:
    password = os.environ.get("KC_PASSWORD")
    if password is None:
        print("kcpassword.py: KC_PASSWORD environment variable not set", file=sys.stderr)
        return 1
    data = obfuscate(password)
    # Write atomically with restrictive permissions.
    fd = os.open(KCPASSWORD_PATH, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    try:
        os.write(fd, data)
    finally:
        os.close(fd)
    os.chmod(KCPASSWORD_PATH, 0o600)
    return 0


if __name__ == "__main__":
    sys.exit(main())
