#!/usr/bin/env bash

set -euo pipefail

repo_root() {
  local dir
  dir="$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")"
  echo "$dir/.."
}

log() {
  echo "$@"
}

require_dir() {
  local path="$1"
  if [[ ! -d "$path" ]]; then
    echo "missing directory: $path" >&2
    exit 1
  fi
}

pcb_examples_dir() {
  local root
  root="$(repo_root)"
  echo "$root/PetteriAimonen-focus-stack/examples/pcb"
}

mk_tmp() {
  local name="$1"
  local clean="${2:-}"
  local root
  root="$(repo_root)"
  local tmp="$root/_tmp_${name}"
  if [[ "$clean" == "clean" ]]; then
    rm -rf "$tmp"
  fi
  mkdir -p "$tmp"
  echo "$tmp"
}

prepare_pcb() {
  local tmp="$1"
  local examples
  examples="$(pcb_examples_dir)"
  require_dir "$examples"
  mkdir -p "$tmp/pcb"
  cp -r "$examples/pcb_"*".jpg" "$tmp/pcb"
}
