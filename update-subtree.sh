#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")"

# if does not have remote, add it
git config remote.PetteriAimonen.url >&- ||
  git remote add PetteriAimonen https://github.com/PetteriAimonen/focus-stack
git fetch PetteriAimonen

prefix="PetteriAimonen-focus-stack"
if [[ -d "$prefix" ]]; then
  git subtree pull --prefix "$prefix" PetteriAimonen master
else
  git subtree add --prefix "$prefix" PetteriAimonen master
fi


