#!/usr/bin/env bash

set -euo pipefail

case_dir="$(dirname "$(readlink -f "$0")")"
. "$case_dir/../harness.sh"

tmpdir="$(mk_tmp gallery clean)"
log "tmpdir=$tmpdir"

examples="$(pcb_examples_dir)"
require_dir "$examples"

case_root="$tmpdir/test_with_pcb_example"
mkdir -p "$case_root/pcb"
cp -r "$examples/pcb_"*".jpg" "$case_root/pcb"

mkdir "$case_root/pcb/0_about"
mv "$case_root/pcb/pcb_002.jpg" "$case_root/pcb/0_about/"

"$(repo_root)/myphoto-gallery.sh" "$case_root/pcb/pcb_"*".jpg"
"$(repo_root)/myphoto-gallery.sh" --tag pcb --tag macro --path "macro/pcb2" "$case_root/pcb/pcb_006.jpg"
rm -f "$case_root/pcb/pcb_005.jpg.myphoto.toml"

cat > "$case_root/myphoto.toml" << EOF
about = ["pcb/pcb_006.jpg"]
tags = ["parent"]
EOF

cat > "$case_root/pcb/myphoto.toml" << EOF
about = ["0_about/pcb_002.jpg"]
tags = ["common"]
path = "macro/pcb"
EOF

mkdir -p "$tmpdir/gallery"
cd "$tmpdir/gallery"
"$(repo_root)/myphoto-gallery.sh" import ../test_with_pcb_example/
