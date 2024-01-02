#!/usr/bin/env bash
set -euo pipefail
myphotodir="$(dirname "$(readlink -f "$0")")"
exec nix run "$myphotodir"#myphoto -- "$@"
