{
  description = "A flake for my photography stuff";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, ... }:let
    system = "x86_64-linux";

    pkgs = nixpkgs.legacyPackages.${system};
    lib = pkgs.lib;

    t = lib.trivial;
    hl = pkgs.haskell.lib;

    extraLibraries = with pkgs; [glew self.packages.${system}.focus-stack hugin enblend-enfuse imagemagick exiftool];
    project = devTools:
      let addBuildTools = (t.flip hl.addBuildTools) devTools;
          addExtraLibraries = (t.flip hl.addExtraLibraries) extraLibraries;
      in pkgs.haskellPackages.developPackage {
        root = ./.;
        name = "myphoto-unwrapped";
        returnShellEnv = !(devTools == [ ]);

        modifier = (t.flip t.pipe) [
          addBuildTools
          addExtraLibraries
          hl.dontHaddock
          hl.enableStaticLibraries
          hl.justStaticExecutables
          hl.disableLibraryProfiling
          hl.disableExecutableProfiling
        ];
      };

  in {

    packages.${system} = {
      focus-stack = pkgs.focus-stack.overrideDerivation (oldAttrs: {
        version = "master";
        src = ./PetteriAimonen-focus-stack;
      });

      my-focus-stack = 
        with pkgs;
        writeShellApplication {
        name = "my-focus-stack";

        runtimeInputs = [ self.packages."${system}".focus-stack exiftool ];

        text = builtins.readFile ./my-focus-stack.sh; # or "path =" ??
      };

      myphoto-unwrapped = project [];
      myphoto = pkgs.buildEnv {
          name = "myphoto";

          paths = [ self.packages.${system}.myphoto-unwrapped ];
          pathsToLink = [ "/share" ];

          nativeBuildInputs = [ pkgs.makeWrapper ];

          postBuild = ''
            mkdir $out/bin
            makeWrapper ${self.packages.${system}.myphoto-unwrapped}/bin/myphoto $out/bin/myphoto \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries}
          '';
        };
    };

    devShell.${system} = project (with pkgs.haskellPackages; [
      cabal-fmt
      cabal-install
      haskell-language-server
      hlint
      ghcid
    ]);

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
            myphoto
          ]);
        };
      }
    );
  };
}
