#!/usr/bin/env bash

set -euo pipefail

thisdir="$(dirname "$(readlink -f "$0")")/.."
tmpdir="$thisdir/_tmp_gallery"
rm -rf "$tmpdir"
mkdir -p "$tmpdir"

echo "tmpdir=$tmpdir"

mkdir -p "$tmpdir/test_with_pcb_example/pcb"
cp -r "$thisdir/PetteriAimonen-focus-stack/examples/pcb/pcb_"*".jpg" "$tmpdir/test_with_pcb_example/pcb"

mkdir "$tmpdir/test_with_pcb_example/pcb/0_about"
mv "$tmpdir/test_with_pcb_example/pcb/pcb_002.jpg" "$tmpdir/test_with_pcb_example/pcb/0_about/"


"$thisdir/myphoto-gallery.sh" "$tmpdir/test_with_pcb_example/pcb/pcb_"*".jpg"
"$thisdir/myphoto-gallery.sh" --tag pcb --tag macro --path "macro/pcb2" "$tmpdir/test_with_pcb_example/pcb/pcb_006.jpg"
rm -f "$tmpdir/test_with_pcb_example/pcb/pcb_005.jpg.myphoto.toml"

cat > "$tmpdir/test_with_pcb_example/myphoto.toml" << EOF
about = ["pcb/pcb_006.jpg"]
tags = ["parent"]
EOF

cat > "$tmpdir/test_with_pcb_example/pcb/myphoto.toml" << EOF
about = ["0_about/pcb_002.jpg"]
tags = ["common"]
path = "macro/pcb"
EOF

mkdir -p "$tmpdir/gallery"
cd "$tmpdir/gallery"
"$thisdir/myphoto-gallery.sh" import ../test_with_pcb_example/