#!/usr/bin/env bash

set -e

export NIXPKGS_ALLOW_INSECURE=1

curPwd="$(pwd)"
root="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
stackyaml="$root/stack.yaml"

trap times EXIT

if [[ "$1" == "init" ]]; then
    shift
    if [[ "$1" == "--" ]]; then
        shift
    fi
    set -x
    exec "$root/unported/initFotoDir.sh" "$@"
fi

stack --stack-yaml "$stackyaml"\
      build

echo "run:..."
time stack --stack-yaml "$stackyaml" \
      exec -- myphoto-exe \
      "pwd" "$curPwd" \
      "$@"
