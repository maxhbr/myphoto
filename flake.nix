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

    extraLibraries = with pkgs; [
      self.packages.${system}.focus-stack # main aligning and focus stacking
      hugin # provides align_image_stack, for alternative aligning
      glew # for parallel processing
      enblend-enfuse # provides enfuse, for alternative focus stacking
      imagemagick # for composing colages and more
      exiftool # for extracting metadata
      ffmpeg-headless # for extracting imgs from video
    ];
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
          ((t.flip hl.appendBuildFlags) ["--ghc-options=\" -threaded -rtsopts -with-rtsopts=-N\"" "+RTS"])
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

          paths = [ ];
          pathsToLink = [ "/share" ];

          nativeBuildInputs = [ pkgs.makeWrapper ];

          postBuild = ''
            mkdir $out/bin
            makeWrapper ${self.packages.${system}.myphoto-unwrapped}/bin/myphoto-stack $out/bin/myphoto-stack \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries}
            makeWrapper ${self.packages.${system}.myphoto-unwrapped}/bin/myphoto-stack $out/bin/myphoto-stack-inplace \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries} \
              --add-flags "--inplace"
            makeWrapper ${self.packages.${system}.myphoto-unwrapped}/bin/myphoto-stack $out/bin/myphoto-stack-replace \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries} \
              --add-flags "--replace"
            makeWrapper ${self.packages.${system}.myphoto-unwrapped}/bin/myphoto-stack $out/bin/myphoto-stack-dirs \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries} \
              --add-flags "--dirs"
          '';
        };
    };

    apps.${system} = {
      myphoto-stack {
        type = "app";
        program = "${self.packages.myphoto}/bin/myphoto-stack";
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
          home.packages = (with pkgs; [
            gphoto2
            gphoto2fs
            gimp # -with-plugins
            darktable
            geeqie
          ]) ++ (with self.packages.${system}; [ 
            focus-stack
            my-focus-stack
            myphoto
          ]);
        };
      }
    );
  };
}
