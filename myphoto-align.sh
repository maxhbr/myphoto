#!/usr/bin/env bash
set -euo pipefail
myphotodir="$(dirname "$(readlink -f "$0")")"
exec nix run --log-format raw "$myphotodir"#myphoto-align -- "$@"
