#!/usr/bin/env bash

set -euo pipefail

exec "$(dirname "$(readlink -f "$0")")/run.sh" stack_with_pcb_example -- "$@"
