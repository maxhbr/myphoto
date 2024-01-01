let 
  pkgs = import <nixpkgs> { };
  buildTools = (with pkgs.haskellPackages; [ cabal-install ghcid ]);
  extraLibraries = (with pkgs; [glew focus-stack hugin enblend-enfuse imagemagick exiftool]);
  addPostInstall = pkgs.haskell.lib.overrideCabal (drv: { 
    postInstall = ''
      wrapProgram "$out/bin/myphoto" \
        --set PATH ${pkgs.lib.makeBinPath extraLibraries}
      ${drv.postInstall}
    '';
  });
in 
  pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv: (pkgs.haskell.lib.addExtraLibraries (pkgs.haskell.lib.addBuildTools drv buildTools) extraLibraries);
  } // {
    postInstall = ''
      wrapProgram "$out/bin/myphoto" \
        --set PATH ${pkgs.lib.makeBinPath extraLibraries}
    '';
  }
