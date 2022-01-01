{ mkDerivation, base, containers, stdenv, X11, xmobar, xmonad
, xmonad-contrib
}:
mkDerivation {
  pname = "my-xmonad";
  version = "0.1.0.0";
  src = builtins.path {
    name = "my-xmonad";
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
