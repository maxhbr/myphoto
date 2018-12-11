#!/usr/bin/env nix-shell
#! nix-shell -i bash -p packer
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

ROOT="$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")"
cd "$ROOT"

rm myconfig_virtualbox.box || true

packer build packer.json

# vagrant box add myconfig myconfig-virtualbox.box

# vagrant plugin install vagrant-nixos
# vagrant init myconfig
# vagrant up
