{
  description = "A flake for my photography stuff";

  outputs = inputs@{ self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
  in {

    packages."${system}" = {
      focus-stack =
        with pkgs;
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
        with pkgs;
        writeShellApplication {
        name = "my-focus-stack";

        runtimeInputs = [ self.packages."${system}".focus-stack exiftool ];

        text = builtins.readFile ./my-focus-stack.sh; # or "path =" ??
      };

      myphoto-unwrapped = 
        let 
          buildTools = (with pkgs.haskellPackages; [ cabal-install ghcid ]);
          extraLibraries = (with pkgs; [glew focus-stack hugin enblend-enfuse imagemagick exiftool]);
          addPostInstall = pkgs.haskell.lib.overrideCabal (drv: { 
            postInstall = ''
              wrapProgram "$out/bin/myphoto" \
                --set PATH ${pkgs.lib.makeBinPath extraLibraries}
              ${drv.postInstall}
            '';
          });
        in 
          pkgs.haskellPackages.developPackage {
            root = ./new.hs;
            modifier = drv: (pkgs.haskell.lib.addExtraLibraries (pkgs.haskell.lib.addBuildTools drv buildTools) extraLibraries);
          } // {
            postInstall = ''
              wrapProgram "$out/bin/myphoto" \
                --set PATH ${pkgs.lib.makeBinPath extraLibraries}
            '';
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
