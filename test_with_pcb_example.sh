#!/usr/bin/env bash

set -euo pipefail

thisdir="$(dirname "$(readlink -f "$0")")"
tmpdir="$thisdir/_tmp/test_with_pcb_example/"
mkdir -p "$tmpdir"

echo "tmpdir=$tmpdir"

mkdir -p "$tmpdir/pcb"
cp -r "$thisdir/PetteriAimonen-focus-stack/examples/pcb/pcb_"*".jpg" "$tmpdir/pcb"

cd "$tmpdir"
"$thisdir/myphoto.sh" "$@" "$tmpdir/pcb/pcb_"*".jpg"

