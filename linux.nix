{ pkgs, lib, ... }:

let

  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
  all-hies = import (builtins.fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  ghcide-nix = import (builtins.fetchTarball "https://github.com/cachix/ghcide-nix/tarball/master") {};

  myLocation = "home";
  locations = {
    home = {
      lat = 44.9466;
      long = -93.1517;
    };
  };

  latlong = location: if (lib.hasAttrByPath [ location ] locations) then locations.${location} else locations.home;
  background = "$HOME/.config/wallpaper/current";

in
with pkgs;
{

  home.keyboard = {
    layout = "us";
    variant = "altgr-intl";
  };

  fonts.fontconfig.enable = true;

  gtk = {
    enable = true;
    iconTheme = {
      package = pkgs.zafiro-icons;
      name = "Zafiro";
    };
    font = {
      name = "Noto Sans 10";
      package = pkgs.noto-fonts;
    };
    theme = {
      name = "Adwaita-dark";
      package = pkgs.gnome3.gnome-themes-standard;
    };
  };

  home.packages = with pkgs; [
    unstable.cachix
    unstable.postman

    # ghcide-nix.ghcide-ghc865

    # unstable._1password
    # unstable._1password-gui
    bmon                # network monitor
    unstable.burpsuite  # network security tool
    dmenu               # minimal desktop menu
    dropbox
    unstable.exodus     # crypto wallet
    gnome3.cheese       # webcam photos
    # gnome3.gnome-calendar
    # gnome3.gnome-control-center
    google-chrome
    jmtpfs              # Media Transfer Protocol (usb device filesystems)
    unstable.joplin-desktop # notes
    keybase
    keybase-gui
    kitty               # terminal
    libreoffice
    lxmenu-data         # installed apps
    pavucontrol
    qalculate-gtk       # calculator
    qemu
    scowl               # spellchecker / dictionary
    st
    unstable.terraform
    unstable.xdg-utils
    whois
    (xfce.thunar.override { thunarPlugins = with pkgs; [ xfce.thunar-volman xfce.thunar-archive-plugin ]; })
    xfce.xfconf
    xfce.exo
    yubioath-desktop
    yubikey-personalization
    zeal                # docs (like dash)

    # games
    # steam-run
    # unstable.unityhub

    # graphics / print
    # adobe-reader
    blender
    darktable
    ffmpegthumbnailer
    flameshot           # screenshots (PrtSc)
    gimp-with-plugins
    krita
    meshlab
    scribusUnstable     # page layout
    scrot               # CLI screenshotter

    # programming - general
    aws-sam-cli         # AWS serverless app model
    dbeaver             # DB GUI
    docker-compose
    gcc
    ltrace              # lib trace
    strace              # system call trace
    unstable.vscode
    unstable.nixfmt     # format nix
    unstable.uncrustify # format c/c++/c#/java/etc

    # programming - haskell
    unstable.haskellPackages.stylish-haskell
    unstable.ormolu
    unstable.stack

    # chat / email
    unstable.discord
    unstable.signal-desktop
    slack
    unstable.thunderbird-bin
    unstable.zoom-us

    # fonts (format with !column -t)
    aileron            comfortaa              dejavu_fonts
    eunomia            f5_6                   fantasque-sans-mono      ferrum
    fira               fira-code-symbols      fira-mono
    font-awesome       helvetica-neue-lt-std  hermit                   ibm-plex
    inconsolata        iosevka                league-of-moveable-type  liberation_ttf
    libre-baskerville  libre-bodoni           libre-caslon             libre-franklin
    medio              mplus-outline-fonts    national-park-typeface   norwester-font
    penna              route159               seshat
    tenderness         vegur                  vistafonts
    (nerdfonts.override { fonts = [ "FiraCode" "DroidSansMono" ]; })

    # media
    calibre             # e-book library
    evince              # another PDF viewer
    handbrake           # dvd ripper
    mplayer
    mpv
    spotify
    unstable.vlc
  ];

  dconf.enable = false;

  services = {
    blueman-applet.enable = true;

    picom = {
      enable       = true;
      fade         = true;
      fadeDelta    = 5;
      fadeSteps    = ["0.04" "0.04"];
      shadow       = true;
      backend      = "xrender";
      vSync        = true;
      # vSync        = "opengl";
      extraOptions = ''
        glx-no-rebind-pixmap  = true;
        glx-no-stencil        = true;
        # glx-copy-from-front   = false;
        use-damage            = true;
        xrender-sync-fence    = true;
      '';
    };

    gpg-agent = {
      enable           = true;
      defaultCacheTtl  = (60 * 60 * 4);
      enableSshSupport = true;
    };

    mpd.enable = true;

    redshift = {
      enable    = true;
      latitude  = toString (latlong myLocation).lat;
      longitude = toString (latlong myLocation).long;
      tray      = true;
    };

    screen-locker = {
      enable = false;
      inactiveInterval = 15;
      # lockCmd = ''${pkgs.betterlockscreen}/bin/betterlockscreen -u ${background} -l dimblur'';
      # lockCmd = "${pkgs.i3lock-pixeled}/bin/i3lock-pixeled";
      lockCmd = "${pkgs.i3lock-fancy-rapid}/bin/i3lock-fancy-rapid 8 pixel";
    };

    xscreensaver = {
      enable = true;
      settings = {
        lock = true;
      };
    };
  };

  qt = {
    enable = true;
    platformTheme = "gtk"; # gnome or gtk
  };

  xdg = {
    enable = true;
    userDirs.enable = true;
  };

  xsession = {
    enable = true;
    initExtra = ''
      ${pkgs.feh}/bin/feh --bg-fill ${background}

      ${pkgs.networkmanagerapplet}/bin/nm-applet &

      ${pkgs.alsaUtils}/bin/amixer -c0 set Headphone 100%,100%
    '';

    windowManager = import ./xmonad/default.nix pkgs;

    pointerCursor = {
      package = pkgs.gnome3.gnome-themes-standard;
      size = 16; # default = 32; example = 64;
      defaultCursor = "left_ptr"; # example = "X_cursor";
      name = "Adwaita";
    };
  };

  imports = [
    ./xmobar
  ];
}
