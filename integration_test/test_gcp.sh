#!/usr/bin/env bash

# not in `integration_test/cases` because it requires GCP credentials

set -euo pipefail

this_dir="$(dirname "$(readlink -f "$0")")"
. "$this_dir/harness.sh"

tmpdir="$(mk_tmp test_gcp)"
log "tmpdir=$tmpdir"

prepare_pcb "$tmpdir"

cd "$tmpdir"
"$(repo_root)/myphoto-gcp.sh" --many "$tmpdir/pcb"
# "$(repo_root)/myphoto-gcp.sh" "$tmpdir/pcb" "$tmpdir/out"