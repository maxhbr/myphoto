{
  description = "A flake for my photography stuff";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    git-hooks.url = "github:cachix/git-hooks.nix";
  };

  outputs =
    inputs@{ self, nixpkgs, ... }:
    let
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
        libraw # for converting raw files like ARW to tiff, provides dcraw_emu
        libheif # for converting heif files
      ];
      project =
        devTools:
        let
          addBuildTools = (t.flip hl.addBuildTools) devTools;
          addExtraLibraries = (t.flip hl.addExtraLibraries) extraLibraries;
        in
        pkgs.haskellPackages.developPackage {
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
            ((t.flip hl.appendBuildFlags) [
              "--ghc-options=\" -threaded -rtsopts -with-rtsopts=-N\""
              "+RTS"
            ])
          ];
        };

    in
    {

      packages.${system} = {
        focus-stack = pkgs.focus-stack.overrideDerivation (oldAttrs: {
          version = "master";
          src = ./PetteriAimonen-focus-stack;
        });

        myphoto-unwrapped = project [ ];
        myphoto-stack-from-github = pkgs.writeShellScriptBin "myphoto-gh-stack" ''
          exec nix run --refresh "github:maxhbr/myphoto"#myphoto-stack -- "$@"
        '';
        myphoto-watch-from-github = pkgs.writeShellScriptBin "myphoto-gh-watch" ''
          exec nix run --refresh "github:maxhbr/myphoto"#myphoto-watch -- "$@"
        '';
        myphoto = pkgs.buildEnv {
          name = "myphoto";

          paths = [
            self.packages.${system}.myphoto-stack-from-github
            self.packages.${system}.myphoto-watch-from-github
          ];
          pathsToLink = [
            "/bin"
            "/share"
          ];

          nativeBuildInputs = [ pkgs.makeWrapper ];

          postBuild = ''
            mkdir -p $out/bin
            makeWrapper ${self.packages.${system}.myphoto-unwrapped}/bin/myphoto-stack $out/bin/myphoto-stack \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries}
            makeWrapper ${
              self.packages.${system}.myphoto-unwrapped
            }/bin/myphoto-stack $out/bin/myphoto-stack-inplace \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries} \
              --add-flags "--inplace"
            makeWrapper ${
              self.packages.${system}.myphoto-unwrapped
            }/bin/myphoto-stack $out/bin/myphoto-stack-inplace-0 \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries} \
              --add-flags "--inplace" \
              --add-flags "--export"
            makeWrapper ${
              self.packages.${system}.myphoto-unwrapped
            }/bin/myphoto-stack $out/bin/myphoto-stack-replace \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries} \
              --add-flags "--replace"
            makeWrapper ${
              self.packages.${system}.myphoto-unwrapped
            }/bin/myphoto-stack $out/bin/myphoto-stack-replace-0 \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries} \
              --add-flags "--replace" \
              --add-flags "--export"
            makeWrapper ${
              self.packages.${system}.myphoto-unwrapped
            }/bin/myphoto-stack $out/bin/myphoto-stack-dirs \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries} \
              --add-flags "--dirs"

            makeWrapper ${
              self.packages.${system}.myphoto-unwrapped
            }/bin/myphoto-stack $out/bin/myphoto-align \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries} \
              --add-flags "--only-align"

            makeWrapper ${self.packages.${system}.myphoto-unwrapped}/bin/myphoto-watch $out/bin/myphoto-watch \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries}
          '';
        };
        default = self.packages.${system}.myphoto;
      };

      apps.${system} = {
        myphoto-stack = {
          type = "app";
          program = "${self.packages.${system}.myphoto}/bin/myphoto-stack";
        };
        myphoto-watch = {
          type = "app";
          program = "${self.packages.${system}.myphoto}/bin/myphoto-watch";
        };
      };

      formatter.${system} =
        let
          pkgs = nixpkgs.legacyPackages.${system};
          config = self.checks.${system}.pre-commit-check.config;
          inherit (config) package configFile;
          script = ''
            ${pkgs.lib.getExe package} run --all-files --config ${configFile}
          '';
        in
        pkgs.writeShellScriptBin "pre-commit-run" script;
      checks.${system} = {
        pre-commit-check = inputs.git-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            nixfmt-rfc-style.enable = true;
            shfmt.enable = false;
            shfmt.settings.simplify = true;
            # shellcheck.enable = true;
            ormolu.enable = true;
          };
        };
        shell-fmt-check =
          let
            pkgs = inputs.nixpkgs.legacyPackages."${system}";
            files = pkgs.lib.concatStringsSep " " [
              "myphoto-stack.sh"
              "myphoto-watch.sh"
            ];
          in
          pkgs.stdenv.mkDerivation {
            name = "shell-fmt-check";
            src = ./.;
            doCheck = true;
            nativeBuildInputs = with pkgs; [
              shfmt
            ];
            checkPhase = ''
              shfmt -d -s -i 4 -ci ${files}
            '';
            installPhase = ''
              mkdir "$out"
            '';
          };
      };

      devShell.${system} =
        let
          inherit (self.checks.${system}.pre-commit-check) shellHook enabledPackages;
        in
        project (
          with pkgs.haskellPackages;
          [
            cabal-fmt
            cabal-install
            haskell-language-server
            hlint
            ghcid
          ]
          ++ enabledPackages
        );

      homeManagerModules.myphoto = (
        {
          config,
          lib,
          pkgs,
          ...
        }:
        let
          inherit (pkgs.stdenv.hostPlatform) system;
        in
        {
          config = {
            home.packages = (
              with self.packages.${system};
              [
                focus-stack
                myphoto
              ]
            );
          };
        }
      );
    };
}
