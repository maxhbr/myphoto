#!/usr/bin/env bash
# Copyright 2023 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -euo pipefail

main() {
  local in="$1"
  local outBase="$2"

  local imgs=()
  imgs=("${in}/"*)

  local fstImg="${imgs[0]}"
  local fstImgBN
  fstImgBN="$(basename "${fstImg%.*}")"
  local fstImgDate
  fstImgDate="$(exiftool -dateFormat "%Y%m%d" -T -DateTimeOriginal "$fstImg")"

  local lastImg="${imgs[-1]}"
  local lastImgBN
  lastImgBN="$(basename "${lastImg%.*}")"
  local lastImgDate
  lastImgDate="$(exiftool -dateFormat "%Y%m%d" -T -DateTimeOriginal "$lastImg")"

  if [[ "$fstImgDate" != "$lastImgDate" ]]; then
    echo "WARN: the images are from more then one date: $fstImgDate ... $lastImgDate"
  fi

  local out="$outBase/0_${fstImgDate}-${fstImgBN}_to_${lastImgBN}"
  if [[ -d "$out" ]]; then
    echo "the folder $out already exists"
  else
    mkdir -p "$out"
    echo "copy to $out ..."
    cp "${imgs[@]}" "$out"
    echo "... done"
  fi
}
main "$1" "$(pwd)"
