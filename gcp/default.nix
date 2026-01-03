inputs@{ self, nixpkgs, ... }:
system:
let

  pkgs = nixpkgs.legacyPackages.${system};
  lib = pkgs.lib;

in
{
  myphoto-docker-in-gcp = pkgs.writeShellApplication {
    name = "myphoto-docker-in-gcp";
    runtimeInputs = with pkgs; [
      coreutils
      google-cloud-sdk
    ];
    text =
      let
        script = builtins.readFile ./run-myphoto-in-gcp.sh;
      in
      lib.replaceStrings
        [
          "@MYPHOTO_DOCKER@"
          "@REMOTE_SCRIPT@"
        ]
        [
          "${self.packages.${system}.myphoto-docker}"
          "${./myphoto-remote-script.sh}"
        ]
        script;
  };

}
