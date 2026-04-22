#!/usr/bin/env bash

set -euo pipefail

case_dir="$(dirname "$(readlink -f "$0")")"
. "$case_dir/../harness.sh"

tmpdir="$(mk_tmp test_stack_global-chunk-tree-height-2)"
exec &> >(tee "$tmpdir.log")
log "tmpdir=$tmpdir"

prepare_pcb "$tmpdir"

cd "$tmpdir"
"$(repo_root)/myphoto-stack.sh" --export-to-parent --global-chunk-tree-height 2 "$@" "$tmpdir/pcb/pcb_"*".jpg"
