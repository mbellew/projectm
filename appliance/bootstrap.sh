#!/usr/bin/env bash
#
# bootstrap.sh — install Ansible if needed, then apply the appliance playbook to THIS machine.
#
# Usage:
#   ./bootstrap.sh [--check] [extra ansible-playbook args...]
#
# --check runs in Ansible "check mode" (dry run; reports changes without making them).
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PLAYBOOK="${SCRIPT_DIR}/ansible/playbook.yml"
INVENTORY_HOST="localhost,"

CONFIG="${SCRIPT_DIR}/ansible/group_vars/all.yml"
if [[ ! -f "${CONFIG}" ]]; then
  echo "error: ${CONFIG} not found." >&2
  echo "       cp ansible/group_vars/all.yml.example ansible/group_vars/all.yml and edit it first." >&2
  exit 1
fi

install_ansible() {
  if command -v ansible-playbook >/dev/null 2>&1; then
    return
  fi
  echo "Ansible not found; installing..."
  case "$(uname -s)" in
    Darwin)
      if command -v brew >/dev/null 2>&1; then
        brew install ansible
      else
        echo "error: Homebrew not found. Install Homebrew (https://brew.sh) or Ansible manually." >&2
        exit 1
      fi
      ;;
    Linux)
      if command -v apt-get >/dev/null 2>&1; then
        sudo apt-get update && sudo apt-get install -y ansible
      elif command -v dnf >/dev/null 2>&1; then
        sudo dnf install -y ansible
      else
        echo "error: no supported package manager (apt/dnf) found. Install Ansible manually." >&2
        exit 1
      fi
      ;;
    *)
      echo "error: unsupported OS $(uname -s). Install Ansible manually." >&2
      exit 1
      ;;
  esac
}

install_ansible

CHECK=()
EXTRA=()
for arg in "$@"; do
  if [[ "${arg}" == "--check" ]]; then
    CHECK=(--check --diff)
  else
    EXTRA+=("${arg}")
  fi
done

# -K prompts for the sudo (become) password, needed for user creation and auto-login.
exec ansible-playbook \
  -i "${INVENTORY_HOST}" \
  --connection local \
  -K \
  "${CHECK[@]}" \
  "${EXTRA[@]}" \
  "${PLAYBOOK}"
