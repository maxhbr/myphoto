#!/usr/bin/env bash

set -euo pipefail

thisdir="$(dirname "$(readlink -f "$0")")/.."
tmpdir="$thisdir/_tmp_test_docker_watch/"
mkdir -p "$tmpdir"

echo "tmpdir=$tmpdir"

cd "$tmpdir"
mkdir -p "$tmpdir/output"
"$thisdir/run-myphoto-docker.sh" "$thisdir/PetteriAimonen-focus-stack/examples/pcb" "$tmpdir/output"
 
