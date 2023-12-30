{
  description = "A flake for my photography stuff";

  outputs = inputs@{ self, nixpkgs }: let
    system = "x86_64-linux";
  in {

    packages."${system}" = {
      focus-stack =
        with import nixpkgs { system = "x86_64-linux"; };
        stdenv.mkDerivation rec {
          pname = "focus-stack";
          version = "master";

          src = ./PetteriAimonen-focus-stack;

          nativeBuildInputs = [ pkg-config which ronn ];
          buildInputs = [ opencv ];

          makeFlags = [ "prefix=$(out)" ];

          # copied from https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/graphics/focus-stack/default.nix
          meta = with lib; {
            description = "Fast and easy focus stacking";
            homepage = "https://github.com/PetteriAimonen/focus-stack";
            license = licenses.mit;
            maintainers = with maintainers; [ paperdigits ];
          };
        };

      my-focus-stack = 
        with import nixpkgs { system = "x86_64-linux"; };
        writeShellApplication {
        name = "my-focus-stack";

        runtimeInputs = [ self.packages."${system}".focus-stack ];

        text = builtins.readFile ./my-foucs-stack.sh; # or "path =" ??
      };
    };

    homeManagerModules.myphoto = (
      {
        config,
        lib,
        pkgs,
        ...
      }: let
        inherit (pkgs.stdenv.hostPlatform) system;
      in {
        config = {
          home.packages = with pkgs; [
            gphoto2
            gphoto2fs
            gimp # -with-plugins
            darktable
            geeqie
          ] ++ (with self.packages.${system}; [ 
            focus-stack
            my-focus-stack
          ]);
        };
      }
    );
  };
}
