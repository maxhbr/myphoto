#!/usr/bin/env bash

set -euo pipefail

integration_test_dir="$(dirname "$(readlink -f "$0")")"
. "$integration_test_dir/harness.sh"

tmpdir="$(mk_tmp test_gcp)"
log "tmpdir=$tmpdir"

prepare_pcb "$tmpdir"

cd "$tmpdir"
"$(repo_root)/myphoto-gcp.sh" "$tmpdir/pcb" "$tmpdir/out"
