#!/usr/bin/env bash

set -euo pipefail

case_dir="$(dirname "$(readlink -f "$0")")"
. "$case_dir/../harness.sh"

tmpdir="$(mk_tmp test_watch_headless)"
exec &> >(tee "$tmpdir.log")
log "tmpdir=$tmpdir"

examples="$(pcb_examples_dir)"
require_dir "$examples"

cd "$tmpdir"
"$(repo_root)/myphoto-watch.sh" --offset -1 --cluster-distance 60 --once "$examples" --headless -- --no-breaking
 
