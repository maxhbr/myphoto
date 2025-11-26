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
        myphoto-align-from-github = pkgs.writeShellScriptBin "myphoto-gh-align" ''
          exec nix run --refresh "github:maxhbr/myphoto"#myphoto-align -- "$@"
        '';
        myphoto = pkgs.buildEnv {
          name = "myphoto";

          paths = [
            self.packages.${system}.myphoto-stack-from-github
            self.packages.${system}.myphoto-watch-from-github
            self.packages.${system}.myphoto-align-from-github
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

            makeWrapper ${self.packages.${system}.myphoto-unwrapped}/bin/myphoto-watch $out/bin/myphoto-watch \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries}
            makeWrapper ${self.packages.${system}.myphoto-unwrapped}/bin/myphoto-watch $out/bin/myphoto-import \
              --set PATH ${pkgs.lib.makeBinPath extraLibraries} \
              --add-flags "--only-import"
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
            dejavu_fonts
            liberation_ttf
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

            # Create a custom fontconfig that includes system fonts
            mkdir -p $out/etc/fonts
            cat > $out/etc/fonts/fonts.conf << 'EOF'
            <?xml version="1.0"?>
            <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
            <fontconfig>
              <dir>${pkgs.dejavu_fonts}/share/fonts</dir>
              <dir>${pkgs.liberation_ttf}/share/fonts</dir>
              <cachedir>~/.cache/fontconfig</cachedir>
              <include ignore_missing="yes">${pkgs.fontconfig.out}/etc/fonts/fonts.conf</include>
            </fontconfig>
            EOF

            mkdir -p $out/bin

            # Create a wrapper script that properly launches Zerene Stacker
            cat > $out/bin/zerene-stacker << 'EOF'
            #!/bin/sh
            set -e

            # Zerene Stacker installation directory (will be substituted)
            ZS_DIR="@out@/opt/zerene-stacker"

            # Setup environment
            export LD_LIBRARY_PATH="@libPath@:$LD_LIBRARY_PATH"
            export FONTCONFIG_FILE="@out@/etc/fonts/fonts.conf"
            export FONTCONFIG_PATH="@out@/etc/fonts"

            # Zerene Stacker config directory
            ZS_CONFIG_DIR="$HOME/.ZereneStacker"
            mkdir -p "$ZS_CONFIG_DIR"

            # Direct Java invocation (following SampleLaunchProgram.java approach)
            cd "$ZS_DIR"
            exec "$ZS_DIR/jre/bin/java" \
              -Xmx4000m \
              -DjavaBits=64bitJava \
              -Dlaunchcmddir="$ZS_CONFIG_DIR" \
              -classpath "$ZS_DIR/ZereneStacker.jar:$ZS_DIR/JREextensions/*" \
              com.zerenesystems.stacker.gui.MainFrame \
              "$@"
            EOF

            chmod +x $out/bin/zerene-stacker

            # Substitute the nix store paths in the wrapper
            substituteInPlace $out/bin/zerene-stacker \
              --replace '@out@' "$out" \
              --replace '@libPath@' '${pkgs.lib.makeLibraryPath buildInputs}'
          '';

          meta = with pkgs.lib; {
            description = "Focus stacking software for photography";
            homepage = "https://www.zerenesystems.com/";
            # license = pkgs.lib.licenses.unfree;
            platforms = pkgs.lib.platforms.linux;
          };
        };
        zerene-batch = pkgs.writeShellScriptBin "zerene-batch" ''
                    set -euo pipefail
                    
                    # Check if we have input files
                    if [ $# -eq 0 ]; then
                      echo "Usage: zerene-batch [OPTIONS] <image1> <image2> [image3...]"
                      echo "Stacks the provided images using Zerene Stacker"
                      echo ""
                      echo "Options:"
                      echo "  --method=DMAP|PMAX    Stacking method (default: DMAP)"
                      echo "  --output=FILE         Output file path (default: first-input-stacked.tif)"
                      echo "  --align=AUTO|NONE     Alignment method (default: AUTO)"
                      exit 1
                    fi
                    
                    # Parse options
                    METHOD="DMAP"
                    OUTPUT_FILE=""
                    ALIGN="AUTO"
                    FILES=()
                    
                    for arg in "$@"; do
                      case $arg in
                        --method=*)
                          METHOD="''${arg#*=}"
                          ;;
                        --output=*)
                          OUTPUT_FILE="''${arg#*=}"
                          ;;
                        --align=*)
                          ALIGN="''${arg#*=}"
                          ;;
                        *)
                          FILES+=("$arg")
                          ;;
                      esac
                    done
                    
                    if [ ''${#FILES[@]} -eq 0 ]; then
                      echo "Error: No input files provided"
                      exit 1
                    fi
                    
                    
                    # Determine output file
                    if [ -z "$OUTPUT_FILE" ]; then
                      FIRST_FILE="''${FILES[0]}"
                      OUTPUT_DIR=$(dirname "$FIRST_FILE")
                      OUTPUT_NAME=$(basename "$FIRST_FILE" | sed 's/\.[^.]*$//')
                      OUTPUT_FILE="$OUTPUT_DIR/$OUTPUT_NAME-stacked.tif"
                    fi

                    # Create temporary directory for batch processing
                    TEMP_DIR="''${OUTPUT_FILE}.zerene_batch"
                    mkdir -p "$TEMP_DIR"
                    TEMP_DIR=$(readlink -f "$TEMP_DIR")

                    # Make output path absolute
                    OUTPUT_FILE=$(realpath -m "$OUTPUT_FILE")
                    OUTPUT_DIR=$(dirname "$OUTPUT_FILE")
                    
                    # Determine task indicator code based on method and alignment
                    if [ "$ALIGN" = "NONE" ]; then
                      if [ "$METHOD" = "PMAX" ]; then
                        TASK_CODE="3"  # Stack Only (PMax)
                      else
                        TASK_CODE="4"  # Stack Only (DMap)
                      fi
                    else
                      if [ "$METHOD" = "PMAX" ]; then
                        TASK_CODE="1"  # Align and Stack (PMax)
                      else
                        TASK_CODE="2"  # Align and Stack (DMap)
                      fi
                    fi
                    
                    # Build XML batch script following the official format
                    cat > "$TEMP_DIR/batch.xml" << 'XMLEOF'
          <?xml version="1.0" encoding="UTF-8"?>
          <ZereneStackerBatchScript>
            <BatchQueue>
              <Batches length="1">
                <Batch>
                  <Sources length="1">
                    <Source value="%CurrentProject%" />
                  </Sources>
                  <ProjectDispositionCode value="101" />
                  <Tasks length="1">
                    <Task>
                      <OutputImageDispositionCode value="3" />
          XMLEOF
                    
                    echo "            <OutputImageFileName value=\"$OUTPUT_FILE\" />" >> "$TEMP_DIR/batch.xml"
                    
                    cat >> "$TEMP_DIR/batch.xml" << XMLEOF
                      <TaskIndicatorCode value="$TASK_CODE" />
                    </Task>
                  </Tasks>
                </Batch>
              </Batches>
            </BatchQueue>
            <ProjectSpecifications length="1">
              <ProjectSpecification>
          XMLEOF
                    
                    # Add each input file to the XML
                    echo "      <SourceFiles length=\"''${#FILES[@]}\">" >> "$TEMP_DIR/batch.xml"
                    for img in "''${FILES[@]}"; do
                      ABS_PATH=$(realpath "$img")
                      echo "        <SourceFile value=\"$ABS_PATH\" />" >> "$TEMP_DIR/batch.xml"
                    done
                    
                    cat >> "$TEMP_DIR/batch.xml" << 'XMLEOF'
                </SourceFiles>
              </ProjectSpecification>
            </ProjectSpecifications>
          </ZereneStackerBatchScript>
          XMLEOF
                    
                    echo "Running Zerene Stacker batch process..."
                    echo "  Input files: ''${#FILES[@]}"
                    echo "  Method: $METHOD"
                    echo "  Alignment: $ALIGN"
                    echo "  Output: $OUTPUT_FILE"
                    echo "  Batch script: $TEMP_DIR/batch.xml"
                    
                    # Run Zerene Stacker in batch mode
                    ${self.packages.${system}.zerene-stacker}/bin/zerene-stacker \
                      -batchScript "$TEMP_DIR/batch.xml"
                    
                    if [ -f "$OUTPUT_FILE" ]; then
                      echo "Stacking complete: $OUTPUT_FILE"
                    else
                      echo "Error: Output file was not created"
                      echo "Check $TEMP_DIR/batch.xml for the batch script"
                      exit 1
                    fi
        '';
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
        zerene-stacker = {
          type = "app";
          program = "${self.packages.${system}.zerene-stacker}/bin/zerene-stacker";
        };
        zerene-batch = {
          type = "app";
          program = "${self.packages.${system}.zerene-batch}/bin/zerene-batch";
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
