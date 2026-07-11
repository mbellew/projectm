#!/usr/bin/env python3
"""Remove dead custom-wave / custom-shape code from Milkdrop presets.

A preset can carry four custom waves and four custom shapes. Each has an `enabled` flag, and when
that flag is 0 the whole block is inert: the `wavecode_N_*` / `shapecode_N_*` parameters AND the
`wave_N_per_frame*` / `wave_N_per_point*` / `shape_N_per_frame*` code bodies. Presets are usually
saved from an editor that writes every block out regardless, so most files are mostly dead weight
(splitscan was 45% dead).

Two traps this exists to avoid -- both of which caught a human doing it by hand:

  1. Only SOME blocks are usually disabled. rorschach had shapes 0-2 enabled (they draw the
     figure) and only shape 3 disabled. Stripping "all shapecode" would have gutted it.
  2. The code BODIES are named differently from the parameter blocks. Deleting `shapecode_3_*`
     leaves `shape_3_per_frame*` behind -- 75 such lines survived the first pass on splitscan.

It also must NOT touch the built-in waveform's parameters (`wave_r`, `wave_x`, ...), which look
similar but carry no index: only `wave_<digit>_` and `shape_<digit>_` are custom-wave/shape code.

Usage:
    strip-dead-preset-code.py PRESET...            # report what is dead (dry run, default)
    strip-dead-preset-code.py --write PRESET...    # rewrite the files in place
"""

import argparse
import re
import sys

# Parameter blocks: wavecode_0_..., shapecode_2_...
BLOCK_RE = re.compile(r"^(wavecode|shapecode)_(\d+)_", re.IGNORECASE)
# Code bodies: wave_0_per_point1=..., shape_2_per_frame3=...
# The digit is what distinguishes these from the built-in waveform's wave_r / wave_x / wave_mode.
BODY_RE = re.compile(r"^(wave|shape)_(\d+)_", re.IGNORECASE)
ENABLED_RE = re.compile(r"^(wavecode|shapecode)_(\d+)_enabled\s*=\s*(\d+)", re.IGNORECASE)


def enabled_map(lines):
    """kind -> {index: enabled_bool}, read from the *_enabled flags."""
    state = {"wavecode": {}, "shapecode": {}}
    for line in lines:
        m = ENABLED_RE.match(line)
        if m:
            kind, index, value = m.group(1).lower(), int(m.group(2)), int(m.group(3))
            state[kind][index] = value != 0
    return state


def is_dead(line, state):
    """True if this line belongs to a wave/shape whose enabled flag is 0."""
    m = BLOCK_RE.match(line)
    if m:
        kind, index = m.group(1).lower(), int(m.group(2))
        return state[kind].get(index, True) is False

    m = BODY_RE.match(line)
    if m:
        # wave_N_* bodies belong to wavecode_N; shape_N_* bodies to shapecode_N.
        kind = "wavecode" if m.group(1).lower() == "wave" else "shapecode"
        index = int(m.group(2))
        # Unknown index (no enabled flag at all) -> leave it alone rather than guess.
        return state[kind].get(index, True) is False

    return False


def process(path, write):
    with open(path, "r", encoding="utf-8", errors="surrogateescape") as handle:
        lines = handle.readlines()

    state = enabled_map(lines)
    kept = [line for line in lines if not is_dead(line, state)]
    removed = len(lines) - len(kept)

    live = [f"{k}_{i}" for k, m in state.items() for i, on in sorted(m.items()) if on]
    summary = (
        f"{path}: {len(lines)} -> {len(kept)} lines (-{removed})"
        f"  live: {', '.join(live) if live else 'none'}"
    )

    if removed and write:
        with open(path, "w", encoding="utf-8", errors="surrogateescape") as handle:
            handle.writelines(kept)
        summary += "  [rewritten]"
    elif removed:
        summary += "  [dry run]"

    print(summary)
    return removed


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("presets", nargs="+", help=".milk files to process")
    parser.add_argument("--write", action="store_true",
                        help="rewrite the files (default is a dry run)")
    args = parser.parse_args()

    total = sum(process(path, args.write) for path in args.presets)
    print(f"\ntotal dead lines: {total}" + ("" if args.write else " (dry run; pass --write)"))
    return 0


if __name__ == "__main__":
    sys.exit(main())
