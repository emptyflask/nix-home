args@{ config, pkgs, lib, ... }:

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
    "bin/popup-calendar.sh" = {
      text = ''
        #!/bin/sh

        BAR_HEIGHT=27  # polybar height
        BORDER_SIZE=0  # border size from your wm settings
        YAD_WIDTH=222  # 222 is minimum possible value
        YAD_HEIGHT=193 # 193 is minimum possible value
        DATE="$(${pkgs.coreutils}/bin/date +"%A  %Y-%m-%d  %I:%M %p")"

        case "$1" in
        --popup)
          if [ "$(${pkgs.xdotool}/bin/xdotool getwindowfocus getwindowname)" = "yad-calendar" ]; then
              exit 0
          fi

          eval "$(${pkgs.xdotool}/bin/xdotool getmouselocation --shell)"
          eval "$(${pkgs.xdotool}/bin/xdotool getdisplaygeometry --shell)"

          # X
          if [ "$((X + YAD_WIDTH / 2 + BORDER_SIZE))" -gt "$WIDTH" ]; then #Right side
              : $((pos_x = WIDTH - YAD_WIDTH - BORDER_SIZE))
          elif [ "$((X - YAD_WIDTH / 2 - BORDER_SIZE))" -lt 0 ]; then #Left side
              : $((pos_x = BORDER_SIZE))
          else #Center
              : $((pos_x = X - YAD_WIDTH / 2))
          fi

          # Y
          if [ "$Y" -gt "$((HEIGHT / 2))" ]; then #Bottom
              : $((pos_y = HEIGHT - YAD_HEIGHT - BAR_HEIGHT - BORDER_SIZE))
          else #Top
              : $((pos_y = BAR_HEIGHT + BORDER_SIZE))
          fi

          ${pkgs.yad}/bin/yad \
            --calendar --undecorated --fixed --close-on-unfocus --no-buttons \
            --width=$YAD_WIDTH --height=$YAD_HEIGHT --posx=$pos_x --posy=$pos_y \
            --class="yad-calendar" --borders=0 >/dev/null &
          ;;
        *)
          echo "$DATE"
          ;;
        esac
      '';
      executable = true;
    };
  };


  imports = [
    ./common.nix
    ./linux.nix
    ./environment.nix
    ./accounts
    ./services/dunst
    ./services/stalonetray
    ./services/spotifyd
    ./programs/alacritty
    ./programs/git
    ./programs/kitty
    ./programs/neomutt
    ./programs/neovim
    ./programs/rofi
    ./programs/st
    ./programs/tmux
    ./programs/vim
    ./programs/zathura
    (import ./programs/zsh args)
    ./xresources
  ];
}
