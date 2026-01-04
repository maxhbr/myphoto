#!/usr/bin/env bash

set -euo pipefail

case_dir="$(dirname "$(readlink -f "$0")")"
. "$case_dir/../harness.sh"

tmpdir="$(mk_tmp test_stack)"
log "tmpdir=$tmpdir"

prepare_pcb "$tmpdir"

cd "$tmpdir"
"$(repo_root)/myphoto-stack.sh" --enfuse-chunk-size 3 "$@" "$tmpdir/pcb/pcb_"*".jpg"
