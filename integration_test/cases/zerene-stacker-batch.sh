#!/usr/bin/env bash

set -euo pipefail

case_dir="$(dirname "$(readlink -f "$0")")"
. "$case_dir/../harness.sh"

tmpdir="$(mk_tmp test_zerene_stacker_batch)"
exec &> >(tee "$tmpdir.log")
log "tmpdir=$tmpdir"

prepare_pcb "$tmpdir"

cd "$tmpdir"
"$(repo_root)/zerene-stacker-batch.sh" "$@" "$tmpdir/pcb/pcb_"*".jpg"
