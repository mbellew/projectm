# projectM Appliance

Turn a computer into a dedicated **projectM visualizer appliance**: it boots, auto-logs-in a
low-privilege user, and launches projectM fullscreen with no interaction. If projectM crashes it
is automatically restarted.

This directory is self-contained and intended to be split out into its own repository. It does
**not** build projectM — it assumes you already have a built `projectM-Test-UI` binary (see the
main repo's `BUILDING.md` / `just build`).

> **Platform:** macOS and Linux are both implemented. Each Ansible role dispatches to a per-OS
> task file (`Darwin.yml` / `Linux.yml`) via `ansible_system`, and the mechanisms differ:
>
> | Piece     | macOS                                       | Linux                                          |
> |-----------|---------------------------------------------|------------------------------------------------|
> | bundle    | relocatable bundle in `~user/projectm` (`deploy-macos`) | system tree in `/opt/projectm` (`deploy-linux`) |
> | autostart | per-user LaunchAgent (`caffeinate`)         | XDG autostart entry running `/opt/projectm/run-projectm.sh` |
> | autologin | `/etc/kcpassword` + `loginwindow` defaults  | GDM `custom.conf` (`AutomaticLogin`, no stored password) |
> | user      | `dscl` local account                        | `ansible.builtin.user` (+ `video`/`render`/`audio` groups) |
>
> Linux webcam capture (V4L2) works, so the seg/depth pipeline runs on the appliance.

## Quick start

**macOS** — build the relocatable bundle into a user's home, then run the playbook:

```bash
just deploy-macos && just deploy-to-user brpl      # bundle -> /Users/brpl/projectm, seed ~/.projectM
cp ansible/group_vars/all.yml.example ansible/group_vars/all.yml
$EDITOR ansible/group_vars/all.yml                 # set user, device prefs, etc.
./bootstrap.sh                                     # add autostart + auto-login (-K prompts for sudo)
```

**Linux** — install the system tree into `/opt/projectm`, then run the playbook:

```bash
just deploy-linux                                  # binary + libs + CUDA runtime + models -> /opt/projectm (prompts for sudo)
cp ansible/group_vars/all.yml.example ansible/group_vars/all.yml
$EDITOR ansible/group_vars/all.yml                 # uncomment the Linux (/opt) lines; set appliance_user
./bootstrap.sh                                     # creates brpl, writes config, XDG autostart, GDM auto-login
```

Preview either without changing anything with `./bootstrap.sh --check`. Then **reboot** — the
machine auto-logs-in the appliance user and projectM starts fullscreen.

## What it configures

| Role        | Action                                                                              |
|-------------|-------------------------------------------------------------------------------------|
| `user`      | Creates a low-privilege local user (skipped if it already exists). Linux adds it to the `video`/`render`/`audio` groups for camera + GPU access. |
| `config`    | Writes `~/.projectM/config.inp` for that user (fullscreen + audio/video prefs, seg/depth model paths). |
| `autostart` | **macOS:** per-user LaunchAgent running `launch/run-projectm.sh` under `caffeinate`. **Linux:** an XDG autostart entry running `/opt/projectm/run-projectm.sh`. Both relaunch projectM only if it **crashes** (a deliberate quit stays quit) — macOS via the LaunchAgent `KeepAlive`-on-crash, Linux via the wrapper's restart loop. |
| `autologin` | **macOS:** automatic login via `/etc/kcpassword`. **Linux:** GDM `custom.conf` (`AutomaticLogin`; no password is stored). |

## Configuration keys (`group_vars/all.yml`)

See `ansible/group_vars/all.yml.example` for the annotated list. The most important:

- `projectm_binary` — absolute path to the built `projectM-Test-UI`.
- `projectm_preset_path` — directory of `.milk` presets (or use `projectm_preset_list`).
- `appliance_user` / `appliance_user_password` — the kiosk account.
- `projectm_audio_devices` / `projectm_video_devices` — semicolon-separated preference lists
  (case-insensitive name substrings). The first present device wins; otherwise the system
  default is used.

## Caveats / honesty

- **Auto-login security:** macOS auto-login stores an obfuscated (not encrypted) copy of the
  account password in `/etc/kcpassword`. Use a dedicated low-privilege account with a throwaway
  password, and never enable this on a machine holding sensitive data. If you'd rather not
  automate it, set `enable_autologin: false` and turn it on manually in
  **System Settings → Users & Groups → Automatically log in as**.
- **FileVault:** auto-login does not work when FileVault is enabled (the disk-unlock login is
  separate). Disable FileVault on the appliance, or accept a manual unlock at boot.
- **First camera/mic permission prompt:** on first launch macOS prompts for camera/microphone
  access for the appliance user. Approve it once while logged in as that user; the grant
  persists. (TCC cannot be pre-granted without MDM.)
- The roles assume they run on the appliance itself (local connection). Running against a remote
  host over SSH is possible but untested.

### Linux notes

- **Auto-login security:** GDM auto-login stores **no** password — `/etc/gdm3/custom.conf` just
  names the user — so it is far less sensitive than macOS kcpassword. Still use a dedicated
  low-privilege account. No camera permission prompt: device access comes from the `video` group
  (the `user` role adds it), so no per-launch approval is needed.
- **`/opt/projectm` is shared + world-readable** (avoids the Ubuntu `0750` cross-home perms issue
  and duplicating ~1.3 GB per user). The launcher puts `/opt/projectm/lib` on `LD_LIBRARY_PATH`;
  no `patchelf`/RPATH rewriting is needed because `LD_LIBRARY_PATH` is searched before RUNPATH.
- **GPU libs are large:** the bundle includes the CUDA/cuDNN runtime (~1 GB) so the seg/depth
  models run on the GPU. `INCLUDE_MODELS=0` / a missing CUDA dir degrade gracefully (CPU seg).
- **Display manager:** the `autologin` role targets **GDM** (`/etc/gdm3` or `/etc/gdm`). For
  LightDM/SDDM, add the equivalent auto-login config to `roles/autologin/tasks/Linux.yml`.
- **Wayland vs Xorg:** the XDG autostart entry inherits the GNOME session environment either way.
  This was developed against GNOME/Xorg; on Wayland the fullscreen SDL window still works.
