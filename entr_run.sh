#!/usr/bin/env bash
set -euo pipefail

cd "$( dirname "${BASH_SOURCE[0]}" )"

entr_task() (
  set -ex
  # cabal clean
  cabal build
  cabal run myphoto -- --help
)
export -f entr_task

while sleep 1; do
  cat <<EOF | entr -dr bash -c entr_task
$0
$(find app)
myphoto.cabal
EOF
done
