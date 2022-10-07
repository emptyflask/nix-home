{ mkDerivation, base, containers, lib, X11, xmobar, xmonad
, xmonad-contrib
}:
mkDerivation {
  pname = "xmonad-config";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers X11 xmobar xmonad xmonad-contrib
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
