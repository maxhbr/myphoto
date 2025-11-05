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
        zerene-stacker = pkgs.stdenv.mkDerivation rec {
          pname = "zerene-stacker";
          version = "2024-11-18-1210";

          src = pkgs.fetchurl {
            url = "https://zerenesystems.com/stacker/downloads/ZS-Linux-Intel-64bit-T2024-11-18-1210.zip";
            sha256 = "0biwhmy1h09n1mply30dbqg7nyykq9m1xd50ylck9rghl8rl3jww";
          };

          nativeBuildInputs = with pkgs; [
            unzip
            makeWrapper
            autoPatchelfHook
          ];

          buildInputs = with pkgs; [
            xorg.libX11
            xorg.libXext
            xorg.libXi
            xorg.libXrender
            xorg.libXtst
            xorg.libXxf86vm
            freetype
            fontconfig
            alsa-lib
            stdenv.cc.cc.lib
          ];

          unpackPhase = ''
            unzip $src
          '';

          installPhase = ''
            mkdir -p $out/opt/zerene-stacker
            cp -r ZereneStacker/* $out/opt/zerene-stacker/

            # Patch the bundled JRE binaries
            find $out/opt/zerene-stacker/jre -type f -executable -exec chmod +x {} \;

            # Make scripts executable
            chmod +x $out/opt/zerene-stacker/ZereneStacker.bsh
            chmod +x $out/opt/zerene-stacker/*.zslinux 2>/dev/null || true

            mkdir -p $out/bin
            # Create a wrapper that uses the bundled JRE
            makeWrapper $out/opt/zerene-stacker/ZereneStacker.bsh $out/bin/zerene-stacker \
              --prefix LD_LIBRARY_PATH : ${pkgs.lib.makeLibraryPath buildInputs} \
              --chdir $out/opt/zerene-stacker
          '';

          meta = with pkgs.lib; {
            description = "Focus stacking software for photography";
            homepage = "https://www.zerenesystems.com/";
            # license = pkgs.lib.licenses.unfree;
            platforms = pkgs.lib.platforms.linux;
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
        zerene-stacker = {
          type = "app";
          program = "${self.packages.${system}.zerene-stacker}/bin/zerene-stacker";
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
