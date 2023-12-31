#!/usr/bin/env bash
# Copyright 2023 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -euo pipefail

run() {
  local args=()
  while [[ "$1" == -* ]]; do
    args+=("$1")
    shift
  done
  local imgs=( "$@" )
  local numImgs="${#imgs[@]}"
  local fstImg="${imgs[0]}"
  local fstImgDir
  fstImgDir="$(dirname "$fstImg")"
  local fstImgBN
  fstImgBN="$(basename "${fstImg%.*}")"
  # local fstImgExtension="$(echo "${fstImg##*.}" | tr '[:upper:]' '[:lower:]')"
  local fstImgDate
  fstImgDate="$(exiftool -dateFormat "%Y%m%d" -T -DateTimeOriginal "$fstImg")"
  local outputBN="${fstImgDate}-${fstImgBN}-stack_of_${numImgs}"

  local outDir
  if [[ "$(basename "$(readlink -f "$fstImgDir")")" == "0_"* ]]; then
    outDir="$fstImgDir/../0_stacked"
    mkdir -p "$outDir"
  else
    outDir="$fstImgDir"
  fi
  local output="$outDir/${outputBN}.png"


  if [[ -e "$output" ]]; then
    echo "$output was already generated"
    return 0
  else
    echo "working on $output ..."
  fi

  local log="${output}.log"
  for (( i=0; i<$numImgs; i++ )); do
    echo "input $i: ${imgs[$i]}" >> "$log"
  done

  time focus-stack \
    --output="${output}"\
    `#--depthmap="${outputBN}.depthmap.png"` \
    `#--3dview="${outputBN}.3dviewpt.png"` \
    --save-steps \
    --jpgquality=100 \
    `#--nocrop --align-keep-size` \
    "${args[@]}" \
    "${imgs[@]}" \
    2>&1 | tee -a "$log"
  if [[ ! -f "$output" ]]; then
    echo "failed to generate $output"
    return 1
  fi

  local lnfile="${fstImgDir}/${outputBN}"
  if [[ "$lnfile" != "$output" && ! -e "$lnfile" ]]; then
    ln -s "$(realpath --relative-to="${fstImgDir}" "${output}")" "${lnfile}"
  fi
}

run_multiple() {
  local args=()
  while [[ "$1" == -* ]]; do
    args+=("$1")
    shift
  done
  for dir in "$@"; do
    (
      shopt -s nullglob
      echo "run for $dir"
      cd "$dir"
      imgs=(MAX*.JPG) # my pattern for images
      run "${args[@]}" "${imgs[@]}" || echo "failed for $dir..."
    )
  done
}

if [[ "$1" == "--dirs" ]]; then
  shift
  run_multiple "$@"
else
  run "$@"
fi
