{ config, lib, pkgs, ... }:

{
  programs.rofi = {
    enable = true;
    pass = {
      enable = true;
      extraConfig = "";
      stores = [];
    };
    plugins = with pkgs; [
      rofi-calc
      rofimoji
    ];
    terminal = "${pkgs.kitty}/bin/kitty";
    theme = "gruvbox-dark";
  };
}
