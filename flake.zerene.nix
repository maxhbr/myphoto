inputs@{ self, nixpkgs, ... }:
system:
let

  pkgs = nixpkgs.legacyPackages.${system};
  lib = pkgs.lib;

  zerene-stacker-script-template = pkgs.writeText "zerene-stacker-script-template" ''
    #!/bin/sh
    set -e

    # go through arguments and make file paths absolute
    ABS_ARGS=()
    for arg in "$@"; do
      case $arg in
        -*)
          ABS_ARGS+=("$arg")
          ;;
        *)
          if [ ! -f "$arg" ] && [ ! -d "$arg" ]; then
            ABS_ARGS+=("$arg")
          else 
            ABS_PATH=$(realpath "$arg")
            ABS_ARGS+=("$ABS_PATH")
          fi
          ;;
      esac
    done

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
      -Xmx16000m \
      -DjavaBits=64bitJava \
      -Dlaunchcmddir="$ZS_CONFIG_DIR" \
      -classpath "$ZS_DIR/ZereneStacker.jar:$ZS_DIR/JREextensions/*" \
      com.zerenesystems.stacker.gui.MainFrame \
      -noSplashScreen \
      @args@ "''${ABS_ARGS[@]}"
  '';

  zerene-pmax-dmap = pkgs.writeText "zerene-pmax-dmap.xml" ''
    <?xml version="1.0" encoding="UTF-8"?>
    <ZereneStackerBatchScript>
      <BatchQueue>
        <Batches length="1">
          <Batch>
            <Sources length="1">
              <Source value="%CurrentProject%" />
            </Sources>

            <!-- Keep project -->
            <ProjectDispositionCode value="101" />

            <Tasks length="2">

              <!-- PMax -->
              <Task>
                <TaskIndicatorCode value="1" />
                <OutputImageDispositionCode value="2" />
                <Preferences>
                  <OutputImageNaming.Template value="zerene-PMax" />
                </Preferences>
              </Task>

              <!-- DMap -->
              <Task>
                <TaskIndicatorCode value="2" />
                <OutputImageDispositionCode value="2" />
                <Preferences>
                  <OutputImageNaming.Template value="zerene-DMap" />
                </Preferences>
              </Task>

            </Tasks>
          </Batch>
        </Batches>
      </BatchQueue>
    </ZereneStackerBatchScript>
  '';

  zerene-pmax-dmap-no-align = pkgs.writeText "zerene-pmax-dmap-no-align.xml" ''
    <?xml version="1.0" encoding="UTF-8"?>
    <ZereneStackerBatchScript>
      <BatchQueue>
        <Batches length="1">
          <Batch>
            <Sources length="1">
              <Source value="%CurrentProject%" />
            </Sources>

            <!-- Keep project -->
            <ProjectDispositionCode value="101" />

            <Tasks length="2">

              <!-- PMax no align -->
              <Task>
                <TaskIndicatorCode value="3" />
                <OutputImageDispositionCode value="2" />
                <Preferences>
                  <OutputImageNaming.Template value="zerene-PMax" />
                </Preferences>
              </Task>

              <!-- DMap no align -->
              <Task>
                <TaskIndicatorCode value="4" />
                <OutputImageDispositionCode value="2" />
                <Preferences>
                  <OutputImageNaming.Template value="zerene-DMap" />
                </Preferences>
              </Task>

            </Tasks>
          </Batch>
        </Batches>
      </BatchQueue>
    </ZereneStackerBatchScript>
  '';
in
{
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

      cp ${zerene-stacker-script-template} $out/bin/zerene-stacker
      substituteInPlace $out/bin/zerene-stacker \
        --replace-fail '@out@' "$out" \
        --replace-fail '@libPath@' '${pkgs.lib.makeLibraryPath buildInputs}' \
        --replace-fail '@args@' ""
      chmod +x $out/bin/zerene-stacker

      cp ${zerene-stacker-script-template} $out/bin/zerene-stacker-batch
      substituteInPlace $out/bin/zerene-stacker-batch \
        --replace-fail '@out@' "$out" \
        --replace-fail '@libPath@' '${pkgs.lib.makeLibraryPath buildInputs}' \
        --replace-fail '@args@' '-exitOnBatchScriptCompletion -batchScript ${zerene-pmax-dmap}'
      chmod +x $out/bin/zerene-stacker-batch

      cp ${zerene-stacker-script-template} $out/bin/zerene-stacker-batch-no-align 
      substituteInPlace $out/bin/zerene-stacker-batch-no-align \
        --replace-fail '@out@' "$out" \
        --replace-fail '@libPath@' '${pkgs.lib.makeLibraryPath buildInputs}' \
        --replace-fail '@args@' '-exitOnBatchScriptCompletion -batchScript ${zerene-pmax-dmap-no-align}'
      chmod +x $out/bin/zerene-stacker-batch-no-align
    '';

    meta = with pkgs.lib; {
      description = "Focus stacking software for photography";
      homepage = "https://www.zerenesystems.com/";
      # license = pkgs.lib.licenses.unfree;
      platforms = pkgs.lib.platforms.linux;
    };
  };
}
