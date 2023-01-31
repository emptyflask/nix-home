{ pkgs, lib, config, ... }:

let
   popupCalendar = pkgs.writeScript "popup-calendar.sh" ''
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

in

{
  xdg.configFile."xmobar/resources" = {
    recursive = true;
    source = ./resources;
  };

  xdg.configFile."xmobar/xmobarrc".text = ''
    Config
       { font             = "Noto Sans Mono 10"
       , additionalFonts  = [ "Font Awesome 5 Free Solid 10"
                            , "Noto Sans Mono Bold 10"
                            ]
       , borderColor      = "white"
       , border           = NoBorder
       , bgColor          = "#282828"
       , fgColor          = "#f9f7dd"
       , alpha            = 192               -- default: 255
       , position         = Static { xpos = 0, ypos = 0, width = 3176, height = 26 }
       , textOffset       = -1                -- default: -1
       , iconOffset       = -1                -- default: -1
       , lowerOnStart     = True
       , pickBroadest     = False             -- default: False
       , persistent       = True
       , hideOnStart      = False
       , iconRoot         = "${config.xdg.configHome}/xmobar/resources/icons"
       , allDesktops      = True              -- default: True
       , overrideRedirect = False             -- default: True
       , commands         =
          [ Run StdinReader
          , Run CoreTemp
             [ "-t", "<fn=1><fc=#928374></fc></fn> <core0>°C"
             , "-H", "70", "-h", "#cc241d"
             , "-L", "40", "-l", "#83a598"
             , "-n", "#98971a"
             ] 50

          , Run Date
             "<action=`${popupCalendar} --popup` button=1><fn=1><fc=#928374></fc></fn></action> <fc=#fe8019>%a %Y-%m-%d %H:%M</fc>"
             "date" 10

          , Run DiskU
             [ ("/", "<fn=1><fc=#928374></fc></fn> <usedp>% used, <free> free") ]
             [] 300

          , Run DynNetwork
             [ "-t", "<fn=1><fc=#928374></fc></fn> <dev> <fn=1><fc=#98971a></fc></fn><rx> <fn=1><fc=#d65d0e></fc></fn><tx>"
             , "-w", "7"
             , "-S", "True"
             , "--", "--devices", "wlp10s0,eno1"
             ] 20

          , Run Memory
             [ "-t", "<fn=1><fc=#928374></fc></fn> <usedratio>% used, <available>M free"] 10

          , Run MultiCpu
             [ "-t", "<autoipat>"
             , "--"
             , "--fallback-icon-pattern", "<icon=load_%%.xpm/>"
             ] 10

          , Run WeatherX "KSTP"
             [ ("clear",                    "◯")
             , ("sunny",                    "<fn=1></fn>")
             , ("mostly clear",             "<fn=1></fn>")
             , ("mostly sunny",             "<fn=1></fn>")
             , ("partly sunny",             "<fn=1></fn>")
             , ("fair",                     "<fn=1></fn>")
             , ("cloudy",                   "<fn=1></fn>")
             , ("overcast",                 "<fn=1></fn>")
             , ("partly cloudy",            "<fn=1></fn>")
             , ("mostly cloudy",            "<fn=1></fn>")
             , ("considerable cloudiness",  "<fn=1></fn>")
             ]
             [ "-t", "<fc=#928374><skyConditionS></fc> St Paul: <tempF>°F <fc=#665c54>|</fc> <fn=1><fc=#928374></fc></fn> <fc=#bdae93><windCardinal> <windMph></fc><fc=#928374>mph</fc> <fc=#bdae93><rh></fc><fc=#928374>ϕ</fc>"
             , "-L", "32", "-H", "86"
             , "--normal", "#b8bb26", "--high", "#fb4934", "--low", "#83a598"
             ] 6000

          , Run Alsa "default" "Master"
             [ "-c", "#cc241d"
             , "-t", "<fn=1><fc=#928374></fc></fn> <volume>% <status>"
             , "--"
             , "--on", ""
             , "--off", "[muted]"
             ]
          ]
       , sepChar = "%"
       , alignSep = "}{"

       , template = " %StdinReader% }\
          \%multicpu% <fc=#665c54>|</fc> %coretemp% <fc=#665c54>|</fc> %memory% <fc=#665c54>|</fc> %disku% <fc=#665c54>|</fc> %dynnetwork% \
          \{ %alsa:default:Master% <fc=#665c54>|</fc> %KSTP% <fc=#665c54>|</fc> %date% "
       }
  '';
}
