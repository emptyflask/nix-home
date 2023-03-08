args@{ config, pkgs, lib, ... }:

{
  nixpkgs.config.allowUnfree = true;

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

  programs = {
    broot.enable        = true; # directory browser

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    emacs.enable        = true;
    firefox.enable      = true;
    fzf.enable          = true;
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
