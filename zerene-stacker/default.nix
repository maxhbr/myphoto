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
    memoryLimitMB=$((TOTAL_MEM_KB * 80 / 100 / 1024))
    # Fallback to 16GB if free command fails
    if [ -z "$memoryLimitMB" ] || [ "$memoryLimitMB" -lt 1000 ]; then
      memoryLimitMB=16000
    fi

    if [ -z "$HOME" ] || [ ! -d "$HOME" ]; then
      ZS_CONFIG_DIR="$(mktemp -d)"
    else
      ZS_CONFIG_DIR="$HOME/.ZereneStacker2"
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

    if [[ ! -f "$ZS_CONFIG_DIR/zerenstk.launchcmd" ]]; then
      touch "$ZS_CONFIG_DIR/zerenstk.cfg"
      touch "$ZS_CONFIG_DIR/zerenstk.launchOK"
      echo '"@out@/opt/zerene-stacker/jre/bin/java" "''${JAVA_ARGS[@]}"' > "$ZS_CONFIG_DIR/zerenstk.launchcmd"
    fi

    ZS_DIR="@out@/opt/zerene-stacker"
    cd "$ZS_DIR"
    set -x
    env
    exec "$ZS_DIR/jre/bin/java" \
      "''${JAVA_ARGS[@]}" \
      @args@ "''${ABS_ARGS[@]}"
  '';

  zerene-stacker-batch = pkgs.writeShellApplication {
    name = "zerene-stacker-batch";
    runtimeInputs = [
      pkgs.coreutils
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
    };
    text = builtins.readFile ./zerene-stacker-batch.sh;
  };
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
