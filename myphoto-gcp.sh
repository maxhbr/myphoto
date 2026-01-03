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

exec nix run "$myphotodir#myphoto-docker-in-gcp" -- \
    --project $PROJECT \
    --region $REGION \
    --zone $ZONE \
    --bucket $BUCKET \
    --input-dir $INPUT_DIR \
    --output-dir $OUTPUT_DIR