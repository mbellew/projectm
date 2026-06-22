# deploy/ — appliance artifact builders

Produce a **self-contained, relocatable** projectM bundle that can be dropped into an appliance
user's folder. Unlike `appliance/` (the Ansible install/kiosk side), these scripts build *from*
the current build tree and vendor every non-system dynamic library so the result runs without the
build tree, Homebrew, or `~/.local` prefixes.

## Build a bundle

```bash
just deploy-macos          # -> dist/projectm/ + dist/projectm-macos-<arch>.tar.gz
just deploy-linux          # -> /opt/projectm  (builds as you; the recipe runs sudo to write /opt)
```

Or call the scripts directly (they take an optional OUT_DIR and several env overrides — see each
script's header):

```bash
INCLUDE_MODELS=0 deploy/deploy-macos.sh /tmp/out
deploy/deploy-linux.sh /tmp/projectm        # stage the Linux tree anywhere (no sudo)
```

### macOS vs Linux model

The two platforms differ by necessity:

- **macOS** builds a **relocatable** bundle (every dylib `install_name_tool`-rewritten to
  `@loader_path`) into `dist/`, then `install-to-appliance.sh` copies it into a user's home.
- **Linux** assembles a **system tree at `/opt/projectm`** directly (no patchelf): the launcher
  puts `/opt/projectm/lib` on `LD_LIBRARY_PATH`, which is searched before the binary's RUNPATH, so
  the bundled `.so`s win with no relocation. It includes the **CUDA/cuDNN runtime (~1 GB)** so the
  seg/depth models run on the GPU. `deploy-linux.sh` is the whole story — there is no separate
  `install-to-appliance` step on Linux (that script is macOS-only / `dscl`-based).

## What goes in the bundle

| Component                | Source                                  |
|--------------------------|-----------------------------------------|
| `bin/projectM-Test-UI`   | `$BUILD_DIR/src/sdl-test-ui/`           |
| `lib/*.dylib`            | build tree + `~/.local/{onnxruntime,depthai-core}` + Homebrew SDL2, relocated |
| `share/projectm/presets/video/` | `presets/video/*.milk`           |
| `share/projectm/palettes/` | `palettes/*.png` — **reference only**, palettes are baked into the binary |
| `dotprojectM/config.inp` | `appliance/config/config.inp.example`   |
| `dotprojectM/textures/`  | `~/.projectM/textures/`                  |
| `dotprojectM/models/`    | `~/.projectM/models/*.onnx`              |

## On the appliance

```bash
tar xzf projectm-macos-arm64.tar.gz
cd projectm
./install.sh        # seeds ~/.projectM; prints the PROJECTM_BINARY / PROJECTM_PRESET_PATH to use
```

## Camera / microphone permission (TCC) — stable signing

The appliance needs camera + mic access. macOS gates that through TCC, keyed to the binary's
**code-signing identity**. An ad-hoc signature changes on every rebuild, so the grant won't stick.
Without an Apple Developer ID, create a **stable self-signed identity once**:

```bash
deploy/make-signing-cert.sh        # creates "projectM Appliance" in your login keychain
```

After that, `deploy-macos.sh` auto-detects and signs with it (override via `SIGN_IDENTITY=…`;
`SIGN_IDENTITY=-` forces ad-hoc). The binary also carries an embedded `Info.plist`
(`src/sdl-test-ui/Info.plist`) with the camera/mic usage strings and a bundle id
(`net.bellew.projectM`) — required or macOS terminates the process when it opens the camera.

You still grant once per appliance user (TCC can't be pre-granted without MDM): log in as that
user, launch projectM, and click **Allow** on the camera + microphone prompts. With the stable
identity the grant then persists across redeploys.

## Install into another user's home (e.g. the kiosk account)

`just deploy-to-user [user=brpl]` runs `deploy/install-to-appliance.sh` under `sudo`: it creates
`~<user>/.projectM/{textures,models,palettes}` owned by that user, copies the bundle into their
home, and seeds `~<user>/.projectM` by running the bundle's `install.sh` **as** that user. Build
the bundle first (`just deploy-macos`). Needs sudo because it writes another user's home.

```bash
just deploy-macos          # build the bundle
just deploy-to-user brpl   # sudo: install it into /Users/brpl + seed ~brpl/.projectM
```

Then point the `appliance/` LaunchAgent's `PROJECTM_BINARY` / `PROJECTM_PRESET_PATH` at the
bundle's `bin/` and `share/projectm/presets/video/`.
