{
  description = "A flake for my photography stuff";

  outputs = inputs@{ self, nixpkgs }: {

    packages.x86_64-linux.focus-stack =
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

    # packages.x86_64-linux.my-focus-stack = pkgs.writeShellScriptBin "my-focus-stack" ''
    #   exec "${self.packages.x86_64-linux.focus-stack}/bin/focus-stack"
    # '';

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
            # my-focus-stack
          ]);
        };
      }
    );
  };
}
