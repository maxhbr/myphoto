FROM nixos/nix
RUN set -x \
 && nix-channel --add https://nixos.org/channels/nixos-23.11 nixpkgs \
 && nix-channel --update \
 && nix-env -iA nixpkgs.stack \
 && mkdir -p "$HOME/.stack" \
 && echo "nix:" >> "$HOME/.stack/config.yaml" \
 && echo "  enable: true" >> "$HOME/.stack/config.yaml"

ENV NIXPKGS_ALLOW_INSECURE=1

WORKDIR /app
ADD stack.yaml stack.yaml.lock package.yaml
ADD . /app
RUN set -x \
 && stack setup \
 && stack build --only-dependencies

ADD . /app
RUN set -x \
 && stack build

WORKDIR /in

ENTRYPOINT /app/docker/entrypoint.sh
