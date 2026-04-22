#!/usr/bin/env bash

set -euo pipefail

exec "$(dirname "$(readlink -f "$0")")/run.sh" stack_global-chunk-tree-height-2 -- "$@"
