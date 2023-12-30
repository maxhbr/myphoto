#!/usr/bin/env bash

stackyaml=/app/stack.yaml
time stack --stack-yaml "$stackyaml" \
  exec -- myphoto-exe \
  "pwd" "/in" \
  "$@"
