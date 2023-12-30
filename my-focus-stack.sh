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
  local fstImgDir="$(dirname "$fstImg")"
  local fstImgBN="$(basename "${fstImg%.*}")"
  # local fstImgExtension="$(echo "${fstImg##*.}" | tr '[:upper:]' '[:lower:]')"
  local fstImgDate="$(exiftool -dateFormat "%Y%m%d" -T -DateTimeOriginal "$fstImg")"
  local outputBN="${fstImgDate}-${fstImgBN}-stack_of_${numImgs}"

  local outDir
  if [[ "$(basename "$(readlink -f "$fstImgDir")")" == "0_"* ]]; then
    outDir="$fstImgDir/../0_stacked"
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

  time focus-stack \
    --output="${output}"\
    `#--depthmap="${outputBN}.depthmap.png"` \
    `#--3dview="${outputBN}.3dviewpt.png"` \
    --save-steps \
    --jpgquality=100 \
    `#--nocrop --align-keep-size` \
    "${args[@]}" \
    "${imgs[@]}"

  local lnfile="${fstImgDir}/${outputBN}"
  if [[ "$lnfile" != "$output" && ! -e "$lnfile" ]]; then
    ln -s "$(realpath --relative-to="${fstImgDir}" "${output}")" "${lnfile}"
  fi

  # if [[ -f "$output" ]]; then
  #   if [[ "$(basename "$(readlink -f "$fstImgDir")")" == "0_"* ]]; then
  #     local outDir="${fstImgDir}/../0_stacked"
  #     mkdir -p "$outDir"
  #     if [[ ! -e "${outDir}/${output}" ]]; then
  #       mv "${output}" "${outDir}/${output}"
  #       ln -s "$(realpath --relative-to="${fstImgDir}" "${outDir}/${output}")" "${output}"
  #     fi
  #   fi
  # fi
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
