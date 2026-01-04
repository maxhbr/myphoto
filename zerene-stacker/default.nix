inputs@{ self, nixpkgs, ... }:
system:
let

  pkgs = nixpkgs.legacyPackages.${system};
  lib = pkgs.lib;

  zerene-stacker-script-template = pkgs.writeText "zerene-stacker-script-template" ''
    #!${pkgs.stdenv.shell}
    set -e

    export PATH="${pkgs.coreutils}/bin:$PATH"
    export PATH="${pkgs.procps}/bin:$PATH"
    export PATH="${pkgs.gawk}/bin:$PATH"
    export LD_LIBRARY_PATH="@libPath@:$LD_LIBRARY_PATH"
    export FONTCONFIG_FILE="@out@/etc/fonts/fonts.conf"
    export FONTCONFIG_PATH="@out@/etc/fonts"

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

    # Compute memory limit as 80% of available memory
    TOTAL_MEM_KB=$(free | awk '/^Mem:/ {print $2}')
    memoryMB=$((TOTAL_MEM_KB / 1024))
    memoryLimitMB=$((memoryMB * 80 / 100))
    # Fallback to 16GB if free command fails
    if [ -z "$memoryLimitMB" ] || [ "$memoryLimitMB" -lt 1000 ]; then
      memoryLimitMB=16000
    fi

    if [ -z "$HOME" ] || [ ! -d "$HOME" ]; then
      ZS_CONFIG_DIR="$(mktemp -d)"
    else
      ZS_CONFIG_DIR="$HOME/.ZereneStacker"
    fi
    mkdir -p "$ZS_CONFIG_DIR"

    JAVA_ARGS=(
      "-Xmx''${memoryLimitMB}m"
      "-DjavaBits=64bitJava"
      "-Dlaunchcmddir=$ZS_CONFIG_DIR"
      "-classpath"
      "@out@/opt/zerene-stacker/ZereneStacker.jar:@out@/opt/zerene-stacker/JREextensions/*"
      "com.zerenesystems.stacker.gui.MainFrame"
      "-noSplashScreen"
    )

    if [[ ! -f "$ZS_CONFIG_DIR/zerenstk.cfg" ]]; then
      cat <<EOF > "$ZS_CONFIG_DIR/zerenstk.cfg"
    #Zerene Stacker configuration
    #Sun Jan 04 12:21:36 CET 2026
    AutoUpdateCheck.IntervalInDays=0.0
    AutoUpdateCheck.LastKnownBuild=1.04 Build T2024-11-18-1210
    ColorManagement.InputOption.AssumedProfile=sRGB IEC61966-2.1
    ColorManagement.InputOption=Use_EXIF_and_DCF_rules
    ColorManagement.OutputOption=CopyInput
    ComputerConfiguration.PhysicalMemory=''${memoryMB}
    DepthMapControl.EstimationRadius=10
    DepthMapControl.SaveDepthMapImageDirectory={project}/DepthMaps
    DepthMapControl.SmoothingRadius=5
    DepthMapControl.UsedPixelFractionThreshold=0.5
    LaunchConfiguration.HeapSize.64bitJava=''${memoryLimitMB}
    NewProject.PlacementStrategy=InApplicationDefaultDirectory
    RetouchingBrush.ShowBrushes=false
    RetouchingBrush.Type=Details
    SaveImage.BitsPerColor=8
    SaveImage.CompressionQuality=0.75
    SaveImage.CompressionType=none
    SaveImage.FileType=jpg
    SaveImage.RescaleImageToAvoidOverflow=false
    ScreenLayout.Bounds=0,0,1000,1000
    ScreenLayout.ExtendedState=0
    ScreenLayout.FilesDivider.Location=1281
    ScreenLayout.FontsScale=1.0
    ScreenLayout.MainDivider.Location=265
    ScreenLayout.OutputImageWindow.Bounds=762,0,762,1841
    ScreenLayout.OutputImageWindow.ImageScale=1.0
    ScreenLayout.OutputImageWindow.ImageScaleFitToWindow=false
    ScreenLayout.OutputImageWindow.Maximized=false
    ScreenLayout.ResetLayoutForEachProject=true
    ScreenLayout.ResetScaleForEachProject=true
    ScreenLayout.SourceImageWindow.Bounds=0,0,762,1841
    ScreenLayout.SourceImageWindow.ImageScale=1.0
    ScreenLayout.SourceImageWindow.ImageScaleFitToWindow=false
    ScreenLayout.SourceImageWindow.Maximized=false
    SkewSequence.NumberOfOutputImages=3
    SkewSequence.ShiftXPct.Limit1=-3.0
    SkewSequence.ShiftXPct.Limit2=3.0
    SkewSequence.ShiftYPct.Limit1=0.0
    SkewSequence.ShiftYPct.Limit2=0.0
    Slabbing.FramesPerOverlap=3
    SourceFileChooser.LastDirectory=/tmp
    UIStyle.ShowIntermediateOutputInterval=0.0
    EOF
    fi
    if [[ ! -f "$ZS_CONFIG_DIR/zerenstk.launchcmd" ]]; then
      echo '"@out@/opt/zerene-stacker/jre/bin/java" "'"''${JAVA_ARGS[@]}"'"' |tee "$ZS_CONFIG_DIR/zerenstk.launchcmd"
    fi

    ZS_DIR="@out@/opt/zerene-stacker"
    cd "$ZS_DIR"
    set -x
    env
    exec "$ZS_DIR/jre/bin/java" \
      "''${JAVA_ARGS[@]}" \
      "''${ABS_ARGS[@]}"
  '';

  zerene-stacker-batch = pkgs.writeShellApplication {
    name = "zerene-stacker-batch";
    runtimeInputs = [
      pkgs.coreutils
    ];
    text = builtins.readFile ./zerene-stacker-batch.sh;
  };
  zerene-stacker-batch-headless = pkgs.writeShellApplication ( rec {
    name = "zerene-stacker-batch-headless";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.bash
      pkgs.xvfb-run
      pkgs.xorg.xorgserver
      pkgs.xorg.xkbcomp
      pkgs.xkeyboard_config
      pkgs.xorg.xkeyboardconfig
      pkgs.xorg.fontmiscmisc
      pkgs.dejavu_fonts
      pkgs.fontconfig
    ];
    runtimeEnv = {
      XKB_CONFIG_ROOT = "${pkgs.xkeyboard_config}/share/X11/xkb";
      XKB_BIN_DIR = "${pkgs.xorg.xkbcomp}/bin";
      XORG_FONT_PATH = "${pkgs.xorg.fontmiscmisc}/share/fonts/X11/misc,${pkgs.dejavu_fonts}/share/fonts/truetype";
      XORG_PREFIX = "${pkgs.xorg.xorgserver}";
    };
    text = ''
      #!${pkgs.stdenv.shell}
      set -e
      ${pkgs.xvfb-run}/bin/xvfb-run -a \
        -e /tmp/xvfb-run.log \
        -s "-screen 0 1920x1080x24 -nolisten tcp -ac -xkbdir ${runtimeEnv.XKB_CONFIG_ROOT} -fp ${runtimeEnv.XORG_FONT_PATH}" \
        --wait=5 \
        ${zerene-stacker-batch}/bin/zerene-stacker-batch "$@" || {
          code=$?
          cat /tmp/xvfb-run.log
          exit $code
        }
    '';
  });
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
        --replace-fail '@libPath@' '${pkgs.lib.makeLibraryPath buildInputs}'
      chmod +x $out/bin/zerene-stacker

      makeWrapper ${zerene-stacker-batch}/bin/zerene-stacker-batch \
        $out/bin/zerene-stacker-batch \
        --prefix PATH : $out/bin
      makeWrapper ${zerene-stacker-batch-headless}/bin/zerene-stacker-batch-headless \
        $out/bin/zerene-stacker-batch-headless \
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
