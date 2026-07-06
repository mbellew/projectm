# Compiled-Program Disk Cache — status & handoff

Opt-in cache that skips the dominant per-preset shader cost (linking transpiled GLSL) by
saving each linked program via `glGetProgramBinary` and reloading it with `glProgramBinary`
on the next run. Enabled only when the env var **`PROJECTM_SHADER_CACHE`** points at a
writable directory.

Code: [Shader.cpp](src/libprojectM/Renderer/Shader.cpp) /
[Shader.hpp](src/libprojectM/Renderer/Shader.hpp) (`LoadCachedProgram` / `SaveCachedProgram`,
FNV-1a source hashing, `CacheHeader`), plus a glad regen to core 4.1 in
[vendor/glad](vendor/glad) exposing `glGetProgramBinary` / `glProgramBinary` /
`glProgramParameteri` and `GL_NUM_PROGRAM_BINARY_FORMATS`.

## Status: written, compiles, UNVERIFIED at runtime

The cache logic — the `glGetProgramBinary`→disk→`glProgramBinary` round-trip, header/hash
validation, driver-rejection fallback, temp-file-rename atomicity, and the actual speedup —
**has never executed**, because the only dev machine is macOS.

`ProgramBinarySupported()` returns false when the driver reports zero program-binary formats,
which is exactly the case on **Apple GL**. So on macOS `cacheEnabled` is always false and
`CompileProgram` runs the unchanged compile+link path — the feature is inert and safe here,
but its real behavior can only be exercised on **Linux/NVIDIA** (the AtomMan deploy target).

Safety by construction (not a substitute for testing): any miss, hash mismatch, or driver
rejection falls back to a normal compile+link, so a bad/stale cache can only cost a recompile,
never a wrong or broken shader. Wipe the directory to reset.

## How to verify (on Linux/NVIDIA)

Logging was added specifically to make this observable rather than timing-inferred — set the
projectM log level to **Debug**:

- One-time `LOG_INFO`: `"[Shader] Program binary cache enabled at \"<dir>\""` (or the
  "disabled: zero program binary formats" line if the driver lacks support).
- Per program `LOG_DEBUG`: `"cache hit"` / `"cache miss; compiled and saved"` with running
  `(N hits / M misses)` counts.

Steps:
1. `PROJECTM_SHADER_CACHE=/tmp/pmcache <run projectM>` — first run: all misses, dir fills
   with `*.binshader` files; confirm the "enabled" info line appears.
2. Restart, same presets — expect **hits** and a visibly faster preset-load / shader warm-up.
3. Staleness: edit a preset's shader (or bump the GPU driver) and confirm the changed program
   **rebuilds** (miss) instead of loading a stale binary. `GlContextHash()` folds
   vendor/renderer/GL/GLSL-version strings; per-source FNV-1a + source-length guards cover
   GLSL edits.

## Open items / caveats

- **macOS**: permanently disabled by design (0 binary formats). Don't spend time making it
  work there.
- **Cache growth**: no eviction/size cap — the directory grows with distinct presets. Fine
  for an appliance with a fixed library; add a cap if it's ever pointed at a huge/rotating set.
- **Not wired to a config key** yet — it's env-var only. If it graduates to a shipped feature,
  consider a `Shader Cache Path` config.inp key alongside the other path keys, and seeding/
  clearing it in the deploy bundle (see appliance deploy work).
- **Upstreamability**: this is a libprojectM-level change (the glad regen + Shader.cpp), so
  it's a plausible upstream contribution once verified — keep machine-specific paths out.

_Uncommitted alongside this doc: the Shader.cpp/hpp cache impl + logging and the glad 4.1
regen. Committed together so the next session can pick up at "verify on the AtomMan."_
