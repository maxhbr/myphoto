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

run_gcp() {
    local input_arg="$1"
    local output_arg="${2:-}"
    local use_detach="$3"

    local INPUT_DIR="${input_arg%/}"
    local OUTPUT_DIR="${output_arg:-${INPUT_DIR}_gcp}"

    args=()
    args+=(--project "$PROJECT")
    args+=(--region "$REGION")
    args+=(--zone "$ZONE")
    if [[ $INPUT_DIR == "gs://"* ]]; then
        args+=(--input-bucket "$INPUT_DIR")
    else
        args+=(--input-dir "$INPUT_DIR")
    fi
    args+=(--output-dir "$OUTPUT_DIR")
    if [ "$use_detach" = "yes" ]; then
        args+=(--detach)
    fi

    nix run "$myphotodir#myphoto-docker-in-gcp" -- "${args[@]}"
}

if [[ ${1:-} == "--many" ]]; then
    shift
    for INPUT_ARG in "$@"; do
        run_gcp "$INPUT_ARG" "" "yes"
    done
else
    run_gcp "${1:-}" "${2:-}" "no"
fi
