{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.trayer;

in {
  options = {
    services.trayer = {
      enable = mkEnableOption "trayer system tray";

      package = mkOption {
        default = pkgs.trayer;
        defaultText = literalExample "pkgs.trayer";
        type = types.package;
        example = literalExample "pkgs.trayer";
        description = "The package to use for the trayer binary.";
      };

      config = mkOption {
        type = with types; attrsOf (nullOr (either str (either bool int)));
        description = ''
          trayer configuration as a set of attributes.
        '';
        default = { };
        example = {
          align           = "center";
          alpha           = 127;
          distance        = 0;
          distancefrom    = "top";
          edge            = "bottom";
          expand          = true;
          height          = 26;
          heighttype      = "pixel";
          iconspacing     = 0;
          margin          = 0;
          monitor         = 0;
          padding         = 0;
          SetDockType     = true;
          SetPartialStrut = true;
          tint            = "0xFFFFFFFF";
          transparent     = false;
          width           = 100;
          widthtype       = "percent";
        };
      };

    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = [ cfg.package ];

      systemd.user.services.trayer = {
        Unit = {
          Description = "trayer system tray";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        Install = { WantedBy = [ "graphical-session.target" ]; };

        Service = {
          ExecStart =
            let
              args = concatStringsSep " " (mapAttrsToList (name: value: "--${name} ${toString value}") cfg.config);
            in
              "${cfg.package}/bin/trayer ${args}";

          Restart = "on-failure";
        };
      };
    }

  ]);
}
