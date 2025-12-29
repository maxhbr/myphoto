inputs@{ self, nixpkgs, ... }:
system:
let

  pkgs = nixpkgs.legacyPackages.${system};
  lib = pkgs.lib;

  zerene-stacker-script-template = pkgs.writeText "zerene-stacker-script-template" ''
    #!${pkgs.stdenv.shell}
    set -e

    export PATH="${pkgs.coreutils}/bin:$PATH"

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
            ABS_PATH=$(readlink -f "$arg")
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

  zerene-stacker-batch = pkgs.writeShellScriptBin "zerene-stacker-batch" ''
    set -euo pipefail

    export PATH="${pkgs.coreutils}/bin:$PATH"
    export PATH="${pkgs.xvfb-run}/bin:$PATH"

    help_msg() {
      cat <<EOF >&2
    Usage: $0 [--headless] [--pmax|--pmax-aligned out] [--pmax-unaligned out] [--dmap|--dmap-aligned out] [--dmap-unaligned out] IMG1 IMG2 ...
           $0 [--headless] [--pmax|--pmax-aligned out] [--pmax-unaligned out] [--dmap|--dmap-aligned out] [--dmap-unaligned out] DIR
    EOF
    }

    if [ "$#" -lt 1 ] || [ "$1" == "--help" ]; then
      help_msg
      exit 1
    fi

    HEADLESS="false"
    PMAX_ALIGNED_OUTPUT=""
    PMAX_UNALIGNED_OUTPUT=""
    DMAP_ALIGNED_OUTPUT=""
    DMAP_UNALIGNED_OUTPUT=""
    TASK_LENGTH=0
    EXIT_ARG="-exitOnBatchScriptCompletion"
    POSITIONAL=()
    while [[ $# -gt 0 ]]; do
      key="$1"
      case $key in
        --headless)
          HEADLESS="true"
          shift # past argument
          ;;
        --pmax-aligned|--pmax)
          PMAX_ALIGNED_OUTPUT="$(readlink -f "$2")"
          TASK_LENGTH=$((TASK_LENGTH + 1))
          shift # past argument
          shift # past value
          echo "DEBUG: PMAX_ALIGNED_OUTPUT=$PMAX_ALIGNED_OUTPUT" >&2
          ;;
        --pmax-unaligned)
          PMAX_UNALIGNED_OUTPUT="$(readlink -f "$2")"
          TASK_LENGTH=$((TASK_LENGTH + 1))
          shift # past argument
          shift # past value
          echo "DEBUG: PMAX_UNALIGNED_OUTPUT=$PMAX_UNALIGNED_OUTPUT" >&2
          ;;
        --dmap-aligned|--dmap)
          DMAP_ALIGNED_OUTPUT="$(readlink -f "$2")"
          TASK_LENGTH=$((TASK_LENGTH + 1))
          shift # past argument
          shift # past value
          echo "DEBUG: DMAP_ALIGNED_OUTPUT=$DMAP_ALIGNED_OUTPUT" >&2
          ;;
        --dmap-unaligned)
          DMAP_UNALIGNED_OUTPUT="$(readlink -f "$2")"
          TASK_LENGTH=$((TASK_LENGTH + 1))
          shift # past argument
          shift # past value
          echo "DEBUG: DMAP_UNALIGNED_OUTPUT=$DMAP_UNALIGNED_OUTPUT" >&2
          ;;
        --wait)
          EXIT_ARG=""
          shift # past argument
          ;;
        *)
          POSITIONAL+=("$1") # save it in an array for later
          shift # past argument
          ;;
      esac
    done

    if [ "$TASK_LENGTH" -eq 0 ]; then
      echo "Warning: falling back to default outputs" >&2

      length="''${#POSITIONAL[@]}"

      basenameFirstImage="$(basename "''${POSITIONAL[0]}")"
      basenameLastImage="$(basename "''${POSITIONAL[-1]}")"
      prefix="''${basenameFirstImage%%.*}_to_''${basenameLastImage%%.*}_stack_of_''${length}"

      PMAX_ALIGNED_OUTPUT="$(pwd)/''${prefix}_zerene-PMax.tif"
      if [ "$HEADLESS" == "true" ]; then
        TASK_LENGTH=1
      else
        DMAP_ALIGNED_OUTPUT="$(pwd)/''${prefix}_zerene-DMap.tif"
        TASK_LENGTH=2
      fi
    fi

    if [ "$HEADLESS" == "true" ] && ([ "$DMAP_ALIGNED_OUTPUT" != "" ] || [ "$DMAP_UNALIGNED_OUTPUT" != "" ]); then
      echo "Error: in headless mode, DMap is not supported" >&2
      exit 1
    fi

    mkTask() {
      local taskIndicatorCode="$1"
      local output="$2"
      local outputDir="$(dirname "$output")"
      local outputBasename="$(basename "$output")"
      local outputImageNamingTemplate="''${outputBasename%%.*}"
      local outputFileType="''${output##*.}"
      local doNotAlign="''${3:-false}"
      cat << EOF
      <Task>
        <TaskIndicatorCode value="$taskIndicatorCode" />
        <OutputImageDispositionCode value="5" />
        <OutputImagesDesignatedFolder value="$outputDir" />
        <Preferences>
          <OutputImageNaming.Template value="$outputImageNamingTemplate" />
          <SaveImage.FileType value="$outputFileType" />
          <Slabbing.SaveImage.FileType value="tif" />
    EOF
      if [ "$doNotAlign" = "true" ]; then
      cat << EOF
          <AlignmentControl.AllowRotation value="false" />
          <AlignmentControl.AllowScale value="false" />
          <AlignmentControl.AllowShiftX value="false" />
          <AlignmentControl.AllowShiftY value="false" />
          <AlignmentControl.CorrectBrightness value="false" />
          <AlignmentControl.Order.Automatic value="false" />
    EOF
      fi
      cat << EOF
        </Preferences>
      </Task>
    EOF
    }

    xml=$(mktemp $TMP/zerene-batch-XXXXXX.xml)
    cat << EOF > "$xml"
    <?xml version="1.0" encoding="UTF-8"?>
    <ZereneStackerBatchScript>
      <BatchQueue>
        <Batches length="1">
          <Batch>
            <Sources length="1">
              <Source value="%CurrentProject%" />
            </Sources>

            <ProjectDispositionCode value="101" />

            <Tasks length="$TASK_LENGTH">
    EOF
    if [ -n "$PMAX_ALIGNED_OUTPUT" ]; then
      mkTask "1" "$PMAX_ALIGNED_OUTPUT" >> "$xml"
    fi
    if [ -n "$DMAP_ALIGNED_OUTPUT" ]; then
      mkTask "2" "$DMAP_ALIGNED_OUTPUT" >> "$xml"
    fi
    if [ -n "$PMAX_UNALIGNED_OUTPUT" ]; then
      mkTask "1" "$PMAX_UNALIGNED_OUTPUT" "true" >> "$xml"
      # mkTask "4" "$PMAX_UNALIGNED_OUTPUT" >> "$xml"
      # echo "WARNING: unaligned pmax somehow does not work right now" >&2
    fi
    if [ -n "$DMAP_UNALIGNED_OUTPUT" ]; then
      mkTask "2" "$DMAP_UNALIGNED_OUTPUT" "true" >> "$xml"
      # mkTask "5" "$DMAP_UNALIGNED_OUTPUT" >> "$xml"
      # echo "WARNING: unaligned dmap somehow does not work right now" >&2
    fi
      cat << EOF >> "$xml"
            </Tasks>
          </Batch>
        </Batches>
      </BatchQueue>
    </ZereneStackerBatchScript>
    EOF
    echo "DEBUG: $ zerene-stacker -batchScript $xml $EXIT_ARG ..." >&2
    if [ "$HEADLESS" == "true" ]; then
      echo "DEBUG: running in headless mode" >&2
      exec xvfb-run -a zerene-stacker -batchScript "$xml" $EXIT_ARG "''${POSITIONAL[@]}"
    else 
      exec zerene-stacker -batchScript "$xml" $EXIT_ARG "''${POSITIONAL[@]}"
    fi
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

      makeWrapper ${zerene-stacker-batch}/bin/zerene-stacker-batch \
        $out/bin/zerene-stacker-batch \
        --prefix PATH : $out/bin
    '';

    meta = with pkgs.lib; {
      description = "Focus stacking software for photography";
      homepage = "https://www.zerenesystems.com/";
      # license = pkgs.lib.licenses.unfree;
      platforms = pkgs.lib.platforms.linux;
    };
  };
}
