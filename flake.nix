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
        coreutils # for readlink, dirname, needed by google-cloud-sdk wrappers
        google-cloud-sdk # provides gcloud and gsutil for GCP operations
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

      zerene = import ./zerene-stacker/default.nix inputs system;
      gcp = import ./gcp/default.nix inputs system;
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

            makeWrapper ${self.packages.${system}.myphoto-unwrapped}/bin/myphoto-gcp $out/bin/myphoto-gcp \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries}
          '';
        };
        inherit (zerene) zerene-stacker;
        myphoto-docker =
          let
            XKB_CONFIG_ROOT = "/usr/share/X11/xkb";
            XKB_BIN_DIR = "/usr/bin";
            XORG_FONT_PATH = "${pkgs.xorg.fontmiscmisc}/share/fonts/X11/misc,${pkgs.dejavu_fonts}/share/fonts/truetype";
            XORG_PREFIX = "${pkgs.xorg.xorgserver}";
            # mk_screenshots = ''
            #   make_xwd_screenshots() (
            #     set +x
            #     mkdir -p /output/_xwd
            #     while : ; do
            #       ${pkgs.xorg.xwd}/bin/xwd -root -display $DISPLAY -silent -out /output/screenshot.xwd
            #       ${pkgs.coreutils}/bin/sleep 10
            #     done
            #   )
            #   make_xwd_screenshots &
            # '';
            entrypoint = pkgs.writeShellScriptBin "entrypoint" ''
              #!${pkgs.stdenv.shell}
              set -euo pipefail
              DISPLAY=:99
              export DISPLAY
              exec &> >(${pkgs.coreutils}/bin/tee -a /output/entrypoint.log)
              set -x
              ${XORG_PREFIX}/bin/Xvfb "$DISPLAY" \
                -screen 0 1920x1080x24 \
                -nolisten tcp \
                -ac \
                -xkbdir ${XKB_CONFIG_ROOT} \
                -fp ${XORG_FONT_PATH} \
                2>/tmp/Xvfb.log &
              xvfb_pid=$!
              ${pkgs.coreutils}/bin/sleep 1
              ${self.packages.${system}.myphoto}/bin/myphoto-watch --verbose /input /output "$@"
              kill "$xvfb_pid" 2>/dev/null || true
            '';
          in
          pkgs.dockerTools.buildImage {
            name = "myphoto";
            tag = "latest";
            copyToRoot = [
              pkgs.bash
              pkgs.coreutils
              pkgs.xkeyboard_config
              pkgs.xorg.fontmiscmisc
              pkgs.dejavu_fonts
              pkgs.xorg.xauth
              pkgs.xorg.xkbcomp
              pkgs.xorg.xorgserver
              pkgs.xorg.xwd
            ];
            extraCommands = ''
              mkdir -p input
              mkdir -p output
              mkdir -p tmp root/.X11-unix
              chmod 1777 tmp root/.X11-unix
              mkdir -p usr/bin usr/share/X11
              ln -s ${pkgs.xkeyboard_config}/share/X11/xkb usr/share/X11/xkb
            '';
            config = {
              Labels = {
                "org.opencontainers.image.title" = "myphoto";
              };
              Env = [
                "TMPDIR=/tmp"
                "HOME=/root"
                "XDG_RUNTIME_DIR=/root"
                "LANG=C.UTF-8"
                "LC_ALL=C.UTF-8"
                "XKB_CONFIG_ROOT=${XKB_CONFIG_ROOT}"
                "XORG_FONT_PATH=${XORG_FONT_PATH}"
                "XORG_PREFIX=${XORG_PREFIX}"
              ];
              Entrypoint = [
                "${entrypoint}/bin/entrypoint"
              ];
              Cmd = [
                "--all"
                "--once"
                "--clean"
              ];
              WorkingDir = "/output";
            };
          };
        myphoto-docker-in-gcp = gcp.myphoto-docker-in-gcp;
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
        myphoto-gcp = {
          type = "app";
          program = let
              docker-image = "${self.packages.${system}.myphoto-docker}";
              myphoto-gcp-with-docker-image-argument = pkgs.writeShellScriptBin "myphoto-gcp" ''
                #!${pkgs.stdenv.shell}
                exec ${self.packages.${system}.myphoto}/bin/myphoto-gcp --docker-image "${docker-image}" "$@"
              '';
            in "${myphoto-gcp-with-docker-image-argument}/bin/myphoto-gcp";
        };
        myphoto-docker-in-gcp = {
          type = "app";
          program = "${self.packages.${system}.myphoto-docker-in-gcp}/bin/myphoto-docker-in-gcp";
        };
        zerene-stacker = {
          type = "app";
          program = "${self.packages.${system}.zerene-stacker}/bin/zerene-stacker";
        };
        zerene-stacker-batch = {
          type = "app";
          program = "${self.packages.${system}.zerene-stacker}/bin/zerene-stacker-batch";
        };
        zerene-stacker-batch-headless = {
          type = "app";
          program = "${self.packages.${system}.zerene-stacker}/bin/zerene-stacker-batch-headless";
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
              "myphoto-align.sh"
              "myphoto-toPNG.sh"
              "myphoto-gcp.sh"
              "gcp/run-myphoto-in-gcp.sh"
              "gcp/myphoto-remote-provision.sh"
              "gcp/myphoto-remote-execute.sh"
              "app-gcp/remote/provision.sh"
              "app-gcp/remote/execute.sh"
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
            ormolu
            hlint
            ghcid
          ]
          ++ (with pkgs; [
            shfmt
          ])
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
