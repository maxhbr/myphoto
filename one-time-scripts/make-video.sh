#!/usr/bin/env nix-shell
#!nix-shell -i bash -p imagemagick ffmpeg coreutils

set -euo pipefail

# ---- Validate input ----
if (( $# == 0 )); then
  echo "Usage: $0 <image1> [image2 ...]"
  exit 1
fi

# ---- Config ----
W=3840
H=2160
FPS=30
JOBS=4
OUT=output_4k.mp4
LOG=output_4k.log

export MAGICK_TMPDIR="${MAGICK_TMPDIR:-/tmp}"
LIMIT_MEM="2GiB"
LIMIT_MAP="4GiB"
LIMIT_AREA="256MP"

mkdir -p frames
exec > >(tee -a "$LOG") 2>&1

# ---- Capture input files exactly as provided (after shell expansion) ----
files=( "$@" )
total=${#files[@]}

echo "Processing ${total} images with ${JOBS} parallel magick jobs..."

# ---- Worker (one image → one frame) ----
worker() {
  local idx="$1"
  local f="$2"

  printf 'Processing %s/%s: %s\n' "$idx" "$total" "$f"

  magick \
    -limit memory "$LIMIT_MEM" -limit map "$LIMIT_MAP" -limit area "$LIMIT_AREA" \
    "$f" \
    -auto-orient \
    -resize "${W}x${H}" \
    -background black -gravity center -extent "${W}x${H}" \
    -strip -quality 92 \
    "frames/${idx}.jpg"
}
export -f worker
export W H LIMIT_MEM LIMIT_MAP LIMIT_AREA MAGICK_TMPDIR total

# ---- Build deterministic job list (index + filename) ----
tmp_list="$(mktemp)"
trap 'rm -f "$tmp_list"' EXIT

for i in "${!files[@]}"; do
  printf -v idx "%06d" "$((i+1))"
  printf '%s\0%s\0' "$idx" "${files[$i]}" >> "$tmp_list"
done

# ---- Run 4 parallel ImageMagick jobs ----
time xargs -0 -n2 -P "$JOBS" bash -c 'worker "$1" "$2"' _ < "$tmp_list"

# ---- Encode video: 30 images per second ----
echo "Encoding ${OUT} at ${FPS} fps..."
time ffmpeg -hide_banner -y \
  -framerate "$FPS" -i "frames/%06d.jpg" \
  -c:v libx264 -pix_fmt yuv420p -crf 18 -preset slow \
  "$OUT"

echo "Done."
