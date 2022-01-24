{ mkDerivation, base, containers, stdenv, X11, xmobar, xmonad
, xmonad-contrib
}:
mkDerivation {
  pname = "xmonad-config";
  version = "0.1.0.0";
  src = builtins.path {
    name = "xmonad-config";
    path = ./.;
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers X11 xmobar xmonad xmonad-contrib
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
