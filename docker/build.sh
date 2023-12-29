#!/usr/bin/env bash


set -euo pipefail

cd "$(dirname "$0")/.."
podman build -f ./docker/Dockerfile -t maxhbr/myphoto .

