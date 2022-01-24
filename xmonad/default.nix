{ pkgs, ... }:

with pkgs;

{

  xmonad = {
    enable = true;
    enableContribAndExtras = true;

    extraPackages = haskellPackages: [
      haskellPackages.dbus
      haskellPackages.monad-logger
      haskellPackages.xmonad-contrib
    ];

    config = ./lib/xmonad.hs;

    libFiles = {
      "Colors.hs"     = ./lib/Colors.hs;
      "Keys.hs"       = ./lib/Keys.hs;
      "Layout.hs"     = ./lib/Layout.hs;
      "Logging.hs"    = ./lib/Logging.hs;
      "Managers.hs"   = ./lib/Managers.hs;
      "Workspaces.hs" = ./lib/Workspaces.hs;

      "Paths.hs" = pkgs.writeText "Paths.hs" ''
        module Paths where
        htop      = "${htop}/bin/htop"
        kitty     = "${kitty}/bin/kitty"
        qalculate = "${qalculate-gtk}/bin/qalculate-gtk"
        xmobar    = "${haskellPackages.xmobar}/bin/xmobar"
        zeal      = "${zeal}/bin/zeal"
      '';
    };
  };

}
