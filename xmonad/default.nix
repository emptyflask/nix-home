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
      "CenteredIfSingle.hs" = ./lib/CenteredIfSingle.hs;
      "Colors.hs"           = ./lib/Colors.hs;
      "Keys.hs"             = ./lib/Keys.hs;
      "Layout.hs"           = ./lib/Layout.hs;
      "Logging.hs"          = ./lib/Logging.hs;
      "Managers.hs"         = ./lib/Managers.hs;
      "Workspaces.hs"       = ./lib/Workspaces.hs;

      "Paths.hs" = pkgs.writeText "Paths.hs" ''
        module Paths where
        chrome      = "${google-chrome}/bin/google-chrome-stable"
        htop        = "${htop}/bin/htop"
        kitty       = "${kitty}/bin/kitty"
        qalculate   = "${qalculate-gtk}/bin/qalculate-gtk"
        signal      = "${signal-desktop}/bin/signal-desktop"
        slack       = "${slack}/bin/slack"
        spotify     = "${spotify}/bin/spotify"
        thunderbird = "${thunderbird-bin}/bin/thunderbird"
        xmobar      = "${haskellPackages.xmobar}/bin/xmobar"
        zeal        = "${zeal}/bin/zeal"
      '';
    };
  };

}
