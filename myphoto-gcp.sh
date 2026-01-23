#!/usr/bin/env bash
set -euo pipefail
myphotodir="$(dirname "$(readlink -f "$0")")"

run_gcp() {
    local input_arg="$1"
    local output_arg="${2:-}"
    local use_detach="$3"
    shift 3

    local INPUT_DIR="${input_arg%/}"
    local OUTPUT_DIR="${output_arg:-${INPUT_DIR}_gcp}"

    args=()
    if [[ $INPUT_DIR == "gs://"* ]]; then
        args+=(--input-bucket "$INPUT_DIR")
    else
        args+=(--input-dir "$INPUT_DIR")
    fi
    args+=(--output-dir "$OUTPUT_DIR")
    if [ "$use_detach" = "yes" ]; then
        args+=(--detach)
    fi

    set -x
    nix run "$myphotodir#myphoto-gcp" -- "${args[@]}" "$@"
}

if [[ ${1:-} == "--many" ]]; then
    shift
    for INPUT_ARG in "$@"; do
        run_gcp "$INPUT_ARG" "" "yes"
    done
else
    input_arg="${1}"
    output_arg="${2:-}"
    shift 2
    run_gcp "$input_arg" "$output_arg" "no" "$@"
fi