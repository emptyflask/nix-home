{ pkgs, ... }:

with pkgs;
{
  imports = [ ./trayer.nix ];

  services.trayer = {
    enable = true;
    config = {
      align       = "right";
      alpha       = 64;
      distance    = 2;
      edge        = "top";
      height      = 24;
      iconspacing = 5;
      padding     = 5;
      tint        = "0x282828";
      transparent = true;
      width       = 264;
      widthtype   = "pixel";
    };
  };
}
