#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 2 ]; then
  echo "Usage: $0 <input-dir> <output-dir> [<extra-args>]" >&2
  exit 1
fi

myphotodir="$(dirname "$(readlink -f "$0")")"

INPUT_DIR=$(realpath "$1")
OUTPUT_DIR="$2"
extra_args="${@:3}"

if [ ! -d "$INPUT_DIR" ]; then
  echo "Input directory does not exist: $INPUT_DIR" >&2
  exit 1
fi
mkdir -p "$OUTPUT_DIR"
OUTPUT_DIR=$(realpath "$OUTPUT_DIR")
IMAGE_REF="myphoto:latest"

load_image() (
  local image="$(nix build "$myphotodir#myphoto-docker" --no-link --print-out-paths)"

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

  set -x
  podman load --signature-policy "$policyfile" -i "$image"
)

run_image() (
  local docker_extra_args=""
  if [[ -f "$HOME/.ZereneStacker/LicenseKey.txt" ]]; then
    docker_extra_args+=" -v $HOME/.ZereneStacker/LicenseKey.txt:/root/.ZereneStacker/LicenseKey.txt:ro"
  fi
  set -x
  podman run --rm \
    -v "$INPUT_DIR":/input:ro \
    -v "$OUTPUT_DIR":/output \
    $docker_extra_args \
    "$IMAGE_REF" $extra_args
)

load_image
run_image