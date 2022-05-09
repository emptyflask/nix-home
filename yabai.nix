{ config, pkgs, lib, ... }: {
  home.file.yabai = {
    executable = true;
    target = ".config/yabai/yabairc";
    text = ''
      #!/usr/bin/env sh

      # load scripting addition
      sudo yabai --load-sa
      yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"

      yabai -m config layout bsp
      yabai -m config auto_balance off
      yabai -m config window_topmost on

      yabai -m config top_padding    20
      yabai -m config bottom_padding 20
      yabai -m config left_padding   20
      yabai -m config right_padding  20
      yabai -m config window_gap     20
      yabai -m config external_bar   all:26:0

      yabai -m config window_border on
      yabai -m config window_border_width 2
      yabai -m config active_window_border_color 0xff98971a
      yabai -m config normal_window_border_color 0xff282828

      # rules
      yabai -m rule --add app="^System Preferences$" manage=off

      echo "yabai configuration loaded.."
    '';
  };

  home.file.skhd = {
    target = ".config/skhd/skhdrc";
    text = ''
      # # focus window
      # alt - h : /opt/homebrew/bin/yabai -m window --focus west
      # alt - j : /opt/homebrew/bin/yabai -m window --focus south
      # alt - k : /opt/homebrew/bin/yabai -m window --focus north
      # alt - l : /opt/homebrew/bin/yabai -m window --focus east

      # # swap managed window
      # shift + alt - h : /opt/homebrew/bin/yabai -m window --swap west
      # shift + alt - j : /opt/homebrew/bin/yabai -m window --swap south
      # shift + alt - k : /opt/homebrew/bin/yabai -m window --swap north
      # shift + alt - l : /opt/homebrew/bin/yabai -m window --swap east

      # # fast focus desktop
      # # cmd + alt - x : yabai -m space --focus recent
      # # cmd + alt - 1 : yabai -m space --focus 1

      # # send window to desktop and follow focus
      # # shift + cmd - z : yabai -m window --space next; yabai -m space --focus next
      # # shift + cmd - 2 : yabai -m window --space  2; yabai -m space --focus 2

      # # increase window size
      # shift + alt - a : /opt/homebrew/bin/yabai -m window --resize left:-20:0
      # shift + alt - s : /opt/homebrew/bin/yabai -m window --resize right:-20:0
      # # shift + alt - w : yabai -m window --resize top:0:-20

      # # decrease window size
      # # shift + cmd - s : yabai -m window --resize bottom:0:-20
      # # shift + cmd - w : yabai -m window --resize top:0:20

      # # float / unfloat window and center on screen
      # alt - t : /opt/homebrew/bin/yabai -m window --toggle float; \
      #           /opt/homebrew/bin/yabai -m window --grid 4:4:1:1:2:2

      # # toggle sticky(+float), topmost, picture-in-picture
      # alt - p : /opt/homebrew/bin/yabai -m window --toggle sticky; \
      #           /opt/homebrew/bin/yabai -m window --toggle topmost; \
      #           /opt/homebrew/bin/yabai -m window --toggle pip




      ##########################
      ## Application bindings ##
      ##########################

      # open terminal
      alt - return : ${pkgs.kitty}/bin/kitty --directory $HOME

      alt - q : launchctl kickstart -k "gui/''${UID}/homebrew.mxcl.yabai"

      # shift + alt - q : brew services stop skhd \
      #                   brew services stop yabai

      # program launcher
      # alt - p : open /Applications/Alfred\ 4.app

      # org-capture: alt - `
      # alt - 0x32 : ~/.emacs.d/bin/org-capture

      ####################
      ## Window bindings ##
      #####################

      # focus window
      # here the || was added so the selection cycles and doesn't stop at the end or beginning
      alt - j : yabai -m window --focus next || yabai -m window --focus first
      alt - k : yabai -m window --focus prev || yabai -m window --focus last

      # swap window
      shift + alt - return : yabai -m window --swap west # swap with "main" tile (simply swap it west)
      shift + alt - j : yabai -m window --swap next
      shift + alt - k : yabai -m window --swap prev

      # move window
      shift + cmd - h : yabai -m window --warp west
      shift + cmd - j : yabai -m window --warp north

      # rotate tree
      alt - r : yabai -m space --rotate 90

      # mirror tree y-axis
      alt - y : yabai -m space --mirror y-axis

      # mirror tree x-axis
      alt - x : yabai -m space --mirror x-axis

      # balance size of windows
      shift + alt - 0 : yabai -m space --balance

      # increase window size (this is the hack that gives xmonad like resizing)
      alt - h : expr $(yabai -m query --windows --window | jq .frame.x) \< 20 && yabai -m window --resize right:-60:0 || yabai -m window --resize left:-60:0
      alt - l : expr $(yabai -m query --windows --window | jq .frame.x) \< 20 && yabai -m window --resize right:60:0 || yabai -m window --resize left:60:0
      alt - i : yabai -m window --resize bottom:0:-60
      alt - o : yabai -m window --resize bottom:0:60

      # float / unfloat window and center on screen
      alt - t : yabai -m window --toggle float; \
                yabai -m window --grid 4:4:1:1:2:2

      # toggle window fullscreen zoom TODO applies only to current, change this
      # binding to toggle layout between bsp/fullscreen  ?
      alt - space : yabai -m window --toggle zoom-fullscreen; \
                    yabai -m space --toggle padding

      # toggle layout of desktop
      shift + alt - space : layout="$(yabai -m query --spaces | jq -r 'map(select(.focused == 1))| .[].type')" && \
                    if [ "''${layout}" = "bsp" ];then yabai -m space --layout float;else yabai -m space --layout bsp;fi

      # toggle window split type
      alt - e : yabai -m window --toggle split

      ######################
      ## Desktop Bindings ##
      ######################

      # create desktop, move window and follow focus - uses jq for parsing json (brew install jq)
      shift + cmd - n : yabai -m space --create && \
                        index="$(yabai -m query --spaces --display | jq 'map(select(."native-fullscreen" == 0))[-1].index')" && \
                        yabai -m window --space "''${index}" && \
                        yabai -m space --focus "''${index}"

      # create desktop and follow focus - uses jq for parsing json (brew install jq)
      cmd + alt - n : yabai -m space --create && \
                      index="$(yabai -m query --spaces --display | jq 'map(select(."native-fullscreen" == 0))[-1].index')" && \
                      yabai -m space --focus "''${index}"

      # fast focus desktop
      alt - 0x1B : yabai -m space --focus recent
      alt - 1 : yabai -m space --focus 1
      alt - 2 : yabai -m space --focus 2
      alt - 3 : yabai -m space --focus 3
      alt - 4 : yabai -m space --focus 4
      alt - 5 : yabai -m space --focus 5
      alt - 6 : yabai -m space --focus 6
      alt - 7 : yabai -m space --focus 7
      alt - 8 : yabai -m space --focus 8
      alt - 9 : yabai -m space --focus 9
      alt - 0 : yabai -m space --focus 10

      # index="$(yabai -m query --spaces --display| jq 'map(select(."focused" == 1))[-1].index')" && \
      # yabai -m window --space  "''${index}" && \
      # yabai -m space --focus "''${index}"

      # send window to desktop and follow focus
      shift + alt - 1 : yabai -m window --space  1 && yabai -m space --focus 1
      shift + alt - 2 : yabai -m window --space  2 && yabai -m space --focus 2
      shift + alt - 3 : yabai -m window --space  3 && yabai -m space --focus 3
      shift + alt - 4 : yabai -m window --space  4 && yabai -m space --focus 4
      shift + alt - 5 : yabai -m window --space  5 && yabai -m space --focus 5
      shift + alt - 6 : yabai -m window --space  6 && yabai -m space --focus 6
      shift + alt - 7 : yabai -m window --space  7 && yabai -m space --focus 7
      shift + alt - 8 : yabai -m window --space  8 && yabai -m space --focus 8
      shift + alt - 9 : yabai -m window --space  9 && yabai -m space --focus 9
      shift + alt - 0 : yabai -m window --space 10 && yabai -m space --focus 10

      ######################
      ## Monitor Bindings ##
      ######################

      # focus monitor
      cmd + alt - e : yabai -m display --focus 1
      cmd + alt - w : yabai -m display --focus 2
      cmd + alt - 1 : yabai -m display --focus 1
      cmd + alt - 2 : yabai -m display --focus 2
      # cmd + alt - r : yabai -m display --focus 3
      # cmd + alt - 3 : yabai -m display --focus 3

      # send window to monitor and follow focus
      shift + cmd + alt - e  : yabai -m window --display 1; yabai -m display --focus 1
      shift + cmd + alt - w  : yabai -m window --display 2; yabai -m display --focus 2
      shift + cmd + alt - 1  : yabai -m window --display 1; yabai -m display --focus 1
      shift + cmd + alt - 2  : yabai -m window --display 2; yabai -m display --focus 2
      # shift + cmd + alt - r  : yabai -m window --display 3; yabai -m display --focus 3
      # shift + cmd + alt - 3  : yabai -m window --display 3; yabai -m display --focus 3

      # prevents skhd from monitoring events for listed processes.
      .blacklist [
        "qutebrowser"
      ]
    '';
  };
}
