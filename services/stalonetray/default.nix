{ pkgs, ... }:

with pkgs;
{
  services.stalonetray = {
    enable = true;
    config = {
      geometry = "10x1-0-0";
      icon_size = 20;
      slot_size = 26;
      icon_gravity = "SE";
      skip_taskbar = true;
      background = "#282828";
      transparent = true;
      window_strut = "none";
      dockapp_mode = "none";
    };
  };
}
