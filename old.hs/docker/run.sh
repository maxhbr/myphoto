#!/usr/bin/env bash

curdir="$(pwd)"
podman run -it --rm -v "$curdir:/in" maxhbr/myphoto
