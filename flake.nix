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
        self.packages.${system}.zerene-stacker # for zerene-stacker integration
        hugin # provides align_image_stack, for alternative aligning
        glew # for parallel processing
        enblend-enfuse # provides enfuse, for alternative focus stacking
        imagemagick # for composing colages and more
        exiftool # for extracting metadata
        ffmpeg-headless # for extracting imgs from video
        libraw # for converting raw files like ARW to tiff, provides dcraw_emu
        libheif # for converting heif files
        udisks # for mounting devices
      ];
      project =
        devTools:
        let
          addBuildTools = (t.flip hl.addBuildTools) devTools;
          addExtraLibraries = (t.flip hl.addExtraLibraries) extraLibraries;
        in
        pkgs.haskellPackages.developPackage {
          root = lib.cleanSourceWith {
            src = ./.;
            filter =
              path: type:
              let
                baseName = baseNameOf path;
                relativePath = lib.removePrefix (toString ./. + "/") (toString path);
              in
              !(
                # Exclude shell scripts in root
                (
                  type == "regular"
                  && lib.hasSuffix ".sh" baseName
                  && (builtins.match "[^/]+\\.sh" relativePath) != null
                )
                # Exclude specific directories
                || lib.hasPrefix "one-time-scripts" relativePath
                || lib.hasPrefix "old.hs" relativePath
                || lib.hasPrefix "PetteriAimonen-focus-stack" relativePath
              );
          };
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

      zerene = import ./flake.zerene.nix inputs system;
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
        myphoto-align-from-github = pkgs.writeShellScriptBin "myphoto-gh-align" ''
          exec nix run --refresh "github:maxhbr/myphoto"#myphoto-align -- "$@"
        '';
        myphoto-toPNG-from-github = pkgs.writeShellScriptBin "myphoto-gh-toPNG" ''
          exec nix run --refresh "github:maxhbr/myphoto"#myphoto-toPNG -- "$@"
        '';
        myphoto-gallery-from-github = pkgs.writeShellScriptBin "myphoto-gh-gallery" ''
          exec nix run --refresh "github:maxhbr/myphoto"#myphoto-gallery -- "$@"
        '';
        myphoto = pkgs.buildEnv {
          name = "myphoto";

          paths = [
            self.packages.${system}.myphoto-stack-from-github
            self.packages.${system}.myphoto-watch-from-github
            self.packages.${system}.myphoto-align-from-github
            self.packages.${system}.myphoto-toPNG-from-github
            self.packages.${system}.myphoto-gallery-from-github
            self.packages.${system}.zerene-stacker
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

            makeWrapper ${self.packages.${system}.myphoto-unwrapped}/bin/myphoto-align $out/bin/myphoto-align \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries}

            makeWrapper ${self.packages.${system}.myphoto-unwrapped}/bin/myphoto-toPNG $out/bin/myphoto-toPNG \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries}

            makeWrapper ${
              self.packages.${system}.myphoto-unwrapped
            }/bin/myphoto-gallery $out/bin/myphoto-gallery \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries}

            makeWrapper ${self.packages.${system}.myphoto-unwrapped}/bin/myphoto-watch $out/bin/myphoto-watch \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries}
            makeWrapper ${self.packages.${system}.myphoto-unwrapped}/bin/myphoto-watch $out/bin/myphoto-import \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries} \
              --add-flags "--only-import"
          '';
        };
        inherit (zerene) zerene-stacker;
        myphoto-docker = pkgs.dockerTools.buildImage {
          name = "myphoto";
          tag = "latest";
          # copyToRoot = [ pkgs.dockerTools.caCertificates self.packages.${system}.myphoto ] ++ extraLibraries;
          extraCommands = ''
            mkdir -p input
            mkdir -p output
          '';
          config = {
            Labels = {
              "org.opencontainers.image.title" = "myphoto";
            };
            Entrypoint = [
              "${self.packages.${system}.myphoto}/bin/myphoto-watch"
              "/input"
              "/output"
            ];
            Cmd = [
              "--once"
              "--clean"
            ];
            WorkingDir = "/output";
          };
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
        myphoto-align = {
          type = "app";
          program = "${self.packages.${system}.myphoto}/bin/myphoto-align";
        };
        myphoto-toPNG = {
          type = "app";
          program = "${self.packages.${system}.myphoto}/bin/myphoto-toPNG";
        };
        myphoto-gallery = {
          type = "app";
          program = "${self.packages.${system}.myphoto}/bin/myphoto-gallery";
        };
        zerene-stacker = {
          type = "app";
          program = "${self.packages.${system}.zerene-stacker}/bin/zerene-stacker";
        };
        zerene-stacker-batch = {
          type = "app";
          program = "${self.packages.${system}.zerene-stacker}/bin/zerene-stacker-batch";
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
              "myphoto-gallery.sh"
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
