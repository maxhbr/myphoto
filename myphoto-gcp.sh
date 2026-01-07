#!/usr/bin/env bash
set -euo pipefail
myphotodir="$(dirname "$(readlink -f "$0")")"

# Read the following variables from `~/.myphoto/gcp.env`
if [ -f "$HOME/.myphoto/gcp.env" ]; then
    source "$HOME/.myphoto/gcp.env"
else
    echo "GCP configuration file ~/.myphoto/gcp.env not found." >&2
    exit 1
fi

INPUT_DIR="$1"
OUTPUT_DIR="$2"

args=();
args+=(--project "$PROJECT")
args+=(--region "$REGION")
args+=(--zone "$ZONE")
if [[ "$1" == "gs://"* ]]; then
    args+=(--input-bucket "$INPUT_DIR")
else
    args+=(--input-dir "$INPUT_DIR")
fi
args+=(--output-dir "$OUTPUT_DIR")

exec nix run "$myphotodir#myphoto-docker-in-gcp" -- "${args[@]}"