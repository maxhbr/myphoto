#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 2 ]; then
  echo "Usage: $0 <input-dir> <output-dir>" >&2
  exit 1
fi

INPUT_DIR=$(realpath "$1")
OUTPUT_DIR=$2

if [ ! -d "$INPUT_DIR" ]; then
  echo "Input directory does not exist: $INPUT_DIR" >&2
  exit 1
fi

mkdir -p "$OUTPUT_DIR"
OUTPUT_DIR=$(realpath "$OUTPUT_DIR")

IMAGE_LINK="result-myphoto-docker"
IMAGE_REF="myphoto:latest"

rm -f "$IMAGE_LINK"
nix build .#myphoto-docker -o "$IMAGE_LINK"
podman load < "$IMAGE_LINK"

podman run --rm \
  -v "$INPUT_DIR":/input:ro \
  -v "$OUTPUT_DIR":/output \
  "$IMAGE_REF"
