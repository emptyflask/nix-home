{ pkgs, ... }:
with pkgs;
{
  services.dunst = {
    enable = true;

    iconTheme.package = pkgs.gnome3.adwaita-icon-theme;
    iconTheme.name = "Adwaita";

    settings = {

      urgency_low = {
        frame_color = "#1d2021";
        background = "#282828";
        foreground = "#ebdbb2";
        timeout    = "5";
      };

      urgency_normal = {
        frame_color = "#1d2021";
        background = "#3c3836";
        foreground = "#ffffff";
        timeout    = "10";
      };

      urgency_critical = {
        frame_color = "#1d2021";
        background = "#fabd2f";
        foreground = "#000000";
        timeout    = "0";
      };

      global = {
        # The format of the message.  Possible variables:
        #   %a  appname
        #   %s  summary
        #   %b  body
        #   %i  iconname (including its path)
        #   %I  iconname (without its path)
        #   %p  progress value if set ([  0%] to [100%]) or nothing
        format = ''%a\n<b>%s</b>\n%b\n%p'';

        width = "400";
        origin = "top-right";
        offset = "30x55";

        font                = "Roboto 10";
        markup              = "full";
        sort                = "yes";         # Sort messages by urgency
        indicate_hidden     = "yes";         # Show how many messages are currently hidden (see geometry)
        alignment           = "center";      # Align text left/center/right
        show_age_threshold  = "60";          # Show if message is older than x seconds (-1 to disable)
        word_wrap           = "yes";         # Split notifications into multiple lines if they don't fit into geometry
        ignore_newline      = "no";          # Ignore "\n"
        transparency        = "10";          # The transparency of the window. 0 (opaque) to 100 (transparent) - requires compositing window manager (xcompmgr, compiz, compton, etc)
        shrink              = "no";          # Shrink window if it's smaller than the width (ignored if width is 0)
        monitor             = "0";           # Display notifications on the monitor indicated (0 is default)
        follow              = "none";        # Follow mouse/keyboard/none
        show_indicators     = "no";          # Display indicators for URLs (U) and actions (A)
        line_height         = "0";           # The spacing between lines (forced to height of font at minimum)
        separator_height    = "2";           # Space in pixels between two notifications
        padding             = "8";           # Padding between text and separator
        horizontal_padding  = "8";           # Horizontal padding
        separator_color     = "frame";       # Color for separator: auto/foreground/frame/X color
        icon_position       = "left";        # Align icons left/right/off
        max_icon_size       = "64";

        frame_width = "2";

        notification_limit  = "10";
        idle_threshold      = "120";         # Don't remove messages if the user is idle (no mouse or keyboard input) for longer than idle_threshold seconds
        sticky_history      = "yes";         # Make notifications remain until clicked on (yes) or timeout as normal (no) when recalled from history
        history_length      = "20";          # Maximum amount of notifications kept in history

        dmenu               = ''${pkgs.rofi}/bin/rofi -dmenu -p dunst:'';
        browser             = ''${pkgs.google-chrome}/bin/google-chrome-stable'';
      };

    };
  };
}
