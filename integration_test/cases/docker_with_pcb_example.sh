#!/usr/bin/env bash

set -euo pipefail

case_dir="$(dirname "$(readlink -f "$0")")"
. "$case_dir/../harness.sh"

tmpdir="$(mk_tmp test_docker_watch)"
log "tmpdir=$tmpdir"

examples="$(pcb_examples_dir)"
require_dir "$examples"

cd "$tmpdir"
mkdir -p "$tmpdir/output"
"$(repo_root)/run-myphoto-docker.sh" "$examples" "$tmpdir/output" --offset -1 --cluster-distance 60 --once -- --no-breaking
 
