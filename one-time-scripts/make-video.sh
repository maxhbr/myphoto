#!/usr/bin/env nix-shell
#!nix-shell -i bash -p imagemagick ffmpeg coreutils

set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  make_video.sh [--mode fit|fill] [--jobs N] [--fps N] [--out FILE] [--size WxH] -- <images...>

Examples:
  ./make_video.sh --mode fit  -- *.JPG
  ./make_video.sh --mode fill --jobs 4 --fps 30 --out out.mp4 -- IMG_*.jpg

Modes:
  fit   : letterbox/pillarbox (no cropping) to exactly WxH
  fill  : center-crop to fill exactly WxH (no black bars)
EOF
}

# ---- Defaults ----
MODE="fit"          # fit|fill
JOBS=4
FPS=30
OUT="output_4k.mp4"
W=3840
H=2160
LOG="output_4k.log"

# ---- Parse args ----
images=()
while (( $# )); do
  case "$1" in
    --mode)
      MODE="${2:-}"; shift 2
      ;;
    --jobs)
      JOBS="${2:-}"; shift 2
      ;;
    --fps)
      FPS="${2:-}"; shift 2
      ;;
    --out)
      OUT="${2:-}"; shift 2
      ;;
    --size)
      size="${2:-}"; shift 2
      W="${size%x*}"
      H="${size#*x}"
      ;;
    --help|-h)
      usage; exit 0
      ;;
    --)
      shift
      images+=( "$@" )
      break
      ;;
    -*)
      echo "Unknown option: $1"
      usage
      exit 1
      ;;
    *)
      images+=( "$1" )
      shift
      ;;
  esac
done

if [[ "$MODE" != "fit" && "$MODE" != "fill" ]]; then
  echo "Invalid --mode: $MODE (expected fit or fill)"
  exit 1
fi

if (( ${#images[@]} == 0 )); then
  echo "No images provided."
  usage
  exit 1
fi

# ---- Setup ----
mkdir -p frames
exec > >(tee -a "$LOG") 2>&1

export MAGICK_TMPDIR="${MAGICK_TMPDIR:-/tmp}"
LIMIT_MEM="2GiB"
LIMIT_MAP="4GiB"
LIMIT_AREA="256MP"

total=${#images[@]}

echo "Mode: ${MODE} | Size: ${W}x${H} | FPS: ${FPS} | Jobs: ${JOBS} | Images: ${total}"
echo "Output: ${OUT}"

# ---- Worker ----
worker() {
  local idx="$1"
  local f="$2"
  local mode="$3"

  printf 'Processing %s/%s: %s\n' "$idx" "$total" "$f"

  if [[ "$mode" == "fill" ]]; then
    # Fill: scale to cover then crop to exact size
    magick \
      -limit memory "$LIMIT_MEM" -limit map "$LIMIT_MAP" -limit area "$LIMIT_AREA" \
      "$f" \
      -auto-orient \
      -resize "${W}x${H}^" \
      -gravity center -extent "${W}x${H}" \
      -strip -quality 92 \
      "frames/${idx}.jpg"
  else
    # Fit: scale to fit inside then letterbox to exact size
    magick \
      -limit memory "$LIMIT_MEM" -limit map "$LIMIT_MAP" -limit area "$LIMIT_AREA" \
      "$f" \
      -auto-orient \
      -resize "${W}x${H}" \
      -background black -gravity center -extent "${W}x${H}" \
      -strip -quality 92 \
      "frames/${idx}.jpg"
  fi
}
export -f worker
export W H LIMIT_MEM LIMIT_MAP LIMIT_AREA MAGICK_TMPDIR total

# ---- Build deterministic job list ----
tmp_list="$(mktemp)"
trap 'rm -f "$tmp_list"' EXIT

for i in "${!images[@]}"; do
  printf -v idx "%06d" "$((i+1))"
  printf '%s\0%s\0%s\0' "$idx" "${images[$i]}" "$MODE" >> "$tmp_list"
done

# ---- Run parallel magick ----
time xargs -0 -n3 -P "$JOBS" bash -c 'worker "$1" "$2" "$3"' _ < "$tmp_list"

# ---- Encode (30 images/sec by default) ----
time ffmpeg -hide_banner -y \
  -framerate "$FPS" -i "frames/%06d.jpg" \
  -c:v libx264 -pix_fmt yuv420p -crf 18 -preset slow \
  "$OUT"

echo "Done."
