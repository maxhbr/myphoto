#!/usr/bin/env nix-shell
#! nix-shell -i bash -p imagemagick
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -e

if [[ "$1" == "--help" ]]; then
    cat<<EOF
  $0 img [img [img ...]]
EOF
    exit 0
fi

size=2000

scale() {
    image=$1
    convert "$image"'['"$size"'x]' "${size}_${image}.jpg"
}

for file in "$@"; do
    scale "$file"
done
