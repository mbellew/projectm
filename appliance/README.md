# projectM Appliance

Turn a computer into a dedicated **projectM visualizer appliance**: it boots, auto-logs-in a
low-privilege user, and launches projectM fullscreen with no interaction. If projectM crashes it
is automatically restarted.

This directory is self-contained and intended to be split out into its own repository. It does
**not** build projectM — it assumes you already have a built `projectM-Test-UI` binary (see the
main repo's `BUILDING.md` / `just build`).

> **Platform:** macOS is implemented today. The Ansible roles are OS-gated so a Linux
> (getty/display-manager auto-login + systemd kiosk unit) variant can be added later without
> restructuring. Note that projectM's **video capture is currently macOS-only**; on Linux the
> `Video Devices` preference is plumbed through but inert until a Linux capture backend exists.

## Quick start

1. Copy and edit the configuration:

   ```bash
   cp ansible/group_vars/all.yml.example ansible/group_vars/all.yml
   $EDITOR ansible/group_vars/all.yml      # set the binary path, user, device prefs, etc.
   ```

2. Run the bootstrap (installs Ansible if missing, then applies the playbook to *this* machine):

   ```bash
   ./bootstrap.sh
   ```

   To preview without changing anything:

   ```bash
   ./bootstrap.sh --check
   ```

3. Reboot. The machine auto-logs-in the appliance user and projectM starts fullscreen.

## What it configures

| Role        | Action                                                                              |
|-------------|-------------------------------------------------------------------------------------|
| `user`      | Creates a low-privilege local user (skipped if it already exists).                  |
| `config`    | Writes `~/.projectM/config.inp` for that user (fullscreen + audio/video prefs).     |
| `autostart` | Installs a per-user LaunchAgent that runs `launch/run-projectm.sh` at login, wrapped in `caffeinate`. Relaunches projectM only if it **crashes** (so a deliberate Cmd-Q quits and stays quit). For a fully locked-down kiosk, change `KeepAlive` to `true` in the plist template. |
| `autologin` | Enables macOS automatic login for the appliance user.                               |

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
