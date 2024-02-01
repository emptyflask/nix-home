args@{ config, pkgs, lib, ... }:

{
  nixpkgs.config.allowUnfree = true;

  nixpkgs.config.permittedInsecurePackages =
    lib.optional (pkgs.obsidian.version == "1.5.3") "electron-25.9.0";

  # nixpkgs.config.packageOverrides = self : rec {
  #   blender = self.blender.override {
  #     cudaSupport = true;
  #   };
  # };

  # nixpkgs.overlays = [
  #   (import (builtins.fetchTarball {
  #     url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
  #   }))
  # ];

  nixpkgs.overlays = [
    (final: prev: {
      postman = prev.postman.overrideAttrs(old: rec {
        version = "20230716100528";
        src = final.fetchurl {
          url = "https://web.archive.org/web/${version}/https://dl.pstmn.io/download/latest/linux_64";
          sha256 = "sha256-svk60K4pZh0qRdx9+5OUTu0xgGXMhqvQTGTcmqBOMq8=";

          name = "${old.pname}-${version}.tar.gz";
        };
      });
    })
  ];

  programs = {
    broot.enable = true; # directory browser

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    firefox.enable      = true;
    fzf.enable          = true;

    gh = {
      enable = true;
      extensions = with pkgs; [
        gh-cal
        gh-eco
      ];
      settings = {
        aliases = {
          co = "pr checkout";
          pv = "pr view";
        };
        git-protocol = "https";
      };
    };
    gh-dash = {
      enable = true;
    };

    go.enable           = true;
    home-manager.enable = true;
    keychain.enable     = true;

    z-lua = {       # directory quick nav
      enable        = true;
      enableAliases = true;
      options       = ["enhanced" "once" "fzf"];
    };
  };

  home.file = {
    ".ghci".source = ./home/ghci;
    ".psqlrc".source = ./home/psqlrc;
    ".railsrc".source = ./home/railsrc;
  };


  imports = [
    ./common.nix
    (import ./linux.nix args)
    ./environment.nix
    ./accounts
    ./services/dunst
    ./services/spotifyd
    ./services/trayer
    ./programs/alacritty
    ./programs/git
    ./programs/kitty
    ./programs/neomutt
    ./programs/neovim
    ./programs/rofi
    # ./programs/st
    ./programs/tmux
    ./programs/vim
    ./programs/zathura
    (import ./programs/zsh args)
    ./xresources
  ];

  home.stateVersion = "18.09";
}
