{ pkgs, ... }:

let
  # xmobarSrc = pkgs.fetchgit {
  #   url = "https://codeberg.org/xmobar/xmobar.git";
  #   rev = "26726e092beb0851743c3fb046e82ce323d818e6";
  #   sha256 = "sha256-VT54ZrUazWG6fPNVRLhfXhjdBWgtZyQ8y/i9TIv1RZw=";
  # };

  # myHaskellPackages = pkgs.haskellPackages.override {
  #   overrides = self: super: {
  #     xmobar = pkgs.haskell.lib.overrideCabal super.xmobar {
  #       src = xmobarSrc;
  #     };
  #   };
  # };

in

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
        chrome      = "${pkgs.google-chrome}/bin/google-chrome-stable"
        htop        = "${pkgs.htop}/bin/htop"
        kitty       = "${pkgs.kitty}/bin/kitty"
        qalculate   = "${pkgs.qalculate-gtk}/bin/qalculate-gtk"
        signal      = "${pkgs.signal-desktop}/bin/signal-desktop"
        slack       = "${pkgs.slack}/bin/slack"
        spotify     = "${pkgs.spotify}/bin/spotify"
        thunderbird = "${pkgs.thunderbird-bin}/bin/thunderbird"
        xmobar      = "${pkgs.xmobar}/bin/xmobar"
        zeal        = "${pkgs.zeal}/bin/zeal"
      '';
    };
  };

}
