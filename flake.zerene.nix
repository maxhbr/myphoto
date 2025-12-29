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

  zerene-stacker-batch-script-template = pkgs.writeText "zerene-stacker-batch-script-template" ''
#!/bin/sh
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
  if [ ! -f "$FIRST_FILE" ]; then
    echo "Error: First input file does not exist: $FIRST_FILE"
    exit 1
  fi
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
@out@/bin/zerene-stacker \
  -batchScript "$TEMP_DIR/batch.xml"
        
if [ -f "$OUTPUT_FILE" ]; then
  echo "Stacking complete: $OUTPUT_FILE"
else
  echo "Error: Output file was not created"
  echo "Check $TEMP_DIR/batch.xml for the batch script"
  exit 1
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

      cp ${zerene-stacker-script-template} $out/bin/zerene-stacker-batch
      substituteInPlace $out/bin/zerene-stacker-batch \
        --replace-fail '@out@' "$out" \
        --replace-fail '@libPath@' '${pkgs.lib.makeLibraryPath buildInputs}' \
        --replace-fail '@args@' '-exitOnBatchScriptCompletion -batchScript ${zerene-pmax-dmap}'
      chmod +x $out/bin/zerene-stacker-batch

      cp ${zerene-stacker-batch-script-template} $out/bin/zerene-stacker-batch-custom
      substituteInPlace $out/bin/zerene-stacker-batch-custom \
        --replace-fail '@out@' "$out"
      chmod +x $out/bin/zerene-stacker-batch-custom
    '';

    meta = with pkgs.lib; {
      description = "Focus stacking software for photography";
      homepage = "https://www.zerenesystems.com/";
      # license = pkgs.lib.licenses.unfree;
      platforms = pkgs.lib.platforms.linux;
    };
  };
}
