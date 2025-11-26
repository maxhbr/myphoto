#!/usr/bin/env bash

set -euo pipefail

thisdir="$(dirname "$(readlink -f "$0")")"
tmpdir="$thisdir/_tmp_test_watch/"
mkdir -p "$tmpdir"

echo "tmpdir=$tmpdir"

cd "$tmpdir"
"$thisdir/myphoto-watch.sh" --offset -1 --cluster-distance 60 --once "$thisdir/PetteriAimonen-focus-stack/examples/pcb" -- --no-breaking
 
