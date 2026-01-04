#!/usr/bin/env bash

set -euo pipefail

case_dir="$(dirname "$(readlink -f "$0")")"
. "$case_dir/../harness.sh"

tmpdir="$(mk_tmp test_zerene_stacker_batch_headless)"
log "tmpdir=$tmpdir"

prepare_pcb "$tmpdir"

cd "$tmpdir"
"$(repo_root)/zerene-stacker-batch.sh" --headless "$@" "$tmpdir/pcb/pcb_"*".jpg"
