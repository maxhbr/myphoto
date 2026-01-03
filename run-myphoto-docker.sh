#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 2 ]; then
  echo "Usage: $0 <input-dir> <output-dir> [<extra-args>]" >&2
  exit 1
fi

INPUT_DIR=$(realpath "$1")
OUTPUT_DIR=$(realpath "$2")
extra_args="${@:3}"

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


tmpdir=$(mktemp -d)
trap "rm -rf $tmpdir" EXIT
policyfile="$tmpdir/podman-policy.json"
cat > "$policyfile" <<'JSON'
{
  "default": [{"type": "insecureAcceptAnything"}],
  "transports": {
    "docker-archive": {
      "": [{"type": "insecureAcceptAnything"}]
    }
  }
}
JSON

podman load --signature-policy "$policyfile" -i "$IMAGE_LINK"

podman run --rm \
  -v "$INPUT_DIR":/input:ro \
  -v "$OUTPUT_DIR":/output \
  "$IMAGE_REF" $extra_args