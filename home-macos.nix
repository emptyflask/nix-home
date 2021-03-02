{ config, pkgs, lib, ... }:

{
  nixpkgs.config.allowUnfree = true;

  # nixpkgs.config.packageOverrides = self : rec {
  #   blender = self.blender.override {
  #     cudaSupport = true;
  #   };
  # };

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
    }))
  ];

  programs = {
    broot.enable        = true; # directory browser

    direnv = {
      enable = true;
      enableNixDirenvIntegration = true;
    };

    # emacs.enable        = true;
    # firefox.enable      = true;
    fzf.enable          = true;
    go.enable           = true;
    home-manager.enable = true;
    # keychain.enable     = true;

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
    ./macos.nix
    ./environment.nix
    ./accounts
    ./services/spotifyd
    ./programs/alacritty
    ./programs/git
    ./programs/neomutt
    ./programs/neovim
    ./programs/tmux
    ./programs/vim
    ./programs/zsh
  ];
}
