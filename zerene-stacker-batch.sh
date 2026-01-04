#!/usr/bin/env bash
set -euo pipefail
myphotodir="$(dirname "$(readlink -f "$0")")"
if [[ "$#" -gt 0 && "$1" == "--headless" ]]; then
  shift
  exec nix run "$myphotodir"#zerene-stacker-batch-headless -- "$@"
else
    exec nix run "$myphotodir"#zerene-stacker-batch -- "$@"
fi

