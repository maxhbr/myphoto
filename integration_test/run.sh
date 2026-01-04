#!/usr/bin/env bash

set -euo pipefail

this_dir="$(dirname "$(readlink -f "$0")")"
cases_dir="$this_dir/cases"

usage() {
  cat <<'USAGE'
Usage:
  integration_test/run.sh                 # run all cases
  integration_test/run.sh case_name        # run one case (name without .sh)
  integration_test/run.sh case.sh -- ...   # pass extra args to case script
USAGE
}

if [[ ! -d "$cases_dir" ]]; then
  echo "missing cases dir: $cases_dir" >&2
  exit 1
fi

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

extra=()
cases=()
if [[ "$#" -gt 0 ]]; then
  while [[ "$#" -gt 0 ]]; do
    if [[ "$1" == "--" ]]; then
      shift
      extra=("$@")
      break
    fi
    cases+=("$1")
    shift
  done
fi

if [[ "${#cases[@]}" -eq 0 ]]; then
  while IFS= read -r -d '' path; do
    cases+=("$(basename "$path")")
  done < <(find "$cases_dir" -maxdepth 1 -type f -name '*.sh' -print0 | sort -z)
fi

if [[ "${#cases[@]}" -eq 0 ]]; then
  echo "no case scripts found in $cases_dir" >&2
  exit 1
fi

for case in "${cases[@]}"; do
  if [[ "$case" != *.sh ]]; then
    case="$case.sh"
  fi
  echo
  echo
  echo "=== Running case: $case ==="
  if [[ ! -x "$cases_dir/$case" ]]; then
    echo "missing case: $cases_dir/$case" >&2
    exit 1
  fi
  if ! "$cases_dir/$case" "${extra[@]}"; then
    echo "failed case: $case" >&2
    exit 1
  fi
done
