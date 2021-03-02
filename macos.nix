{ pkgs, ... }:

let
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

in
with pkgs;
{
  home.packages = with pkgs; [
    unstable.cachix

    bat                 # cat clone with syntax highlighting and git integration
    bc                  # cli calculator
    bmon                # network monitor
    fd                  # find entries in filesystem
    fortune
    htop
    jmtpfs              # Media Transfer Protocol (usb device filesystems)
    jq
    killall
    kitty               # terminal
    # libnotify
    # libXScrnSaver
    magic-wormhole      # simple secure file transfer
    mosh                # ssh alternative
    nix-index
    nix-prefetch-git
    nix-zsh-completions
    nixops
    unstable.pandoc     # document converter
    ranger              # CLI file manager
    ripgrep
    shared_mime_info    # recognize file types
    tmux
    translate-shell
    units
    xarchiver

    # graphics / print
    # adobe-reader
    ffmpegthumbnailer
    imagemagick

    # programming - general
    aws-sam-cli         # AWS serverless app model
    exercism
    foreman
    # gcc
    gnumake
    html-tidy           # format html
    niv                 # nix channel config
    sourceHighlight
    shellcheck          # shell script analyzer
    tig                 # git tui frontend
    universal-ctags
    unstable.vscode
    unstable.nixfmt     # format nix
    unstable.uncrustify # format c/c++/c#/java/etc

    # programming - elixir / erlang
    elixir

    # programming - javascript
    nodejs
    yarn

    # programming - haskell
    ghc
    cabal2nix
    cabal-install
    haskellPackages.apply-refact
    haskellPackages.ghcid
    haskellPackages.haskell-language-server
    haskellPackages.hlint
    haskellPackages.yesod
    unstable.haskellPackages.stylish-haskell
    unstable.haskellPackages.xmobar
    unstable.ormolu
    unstable.stack

    # programming - python
    python3Packages.pynvim # for neovim

    # programming - ruby
    bundix
    jekyll
    ruby_2_7
    rubyPackages_2_7.pry

    # programming - rust
    cargo
    rustc
    rustfmt

    # chat / email
    neomutt             # CLI mail
    protonmail-bridge
    weechat

    # fonts (format with !column -t)
    aileron            comfortaa              dejavu_fonts
    eunomia            f5_6                   fantasque-sans-mono      ferrum
    fira               fira-code              fira-code-symbols        fira-mono
    font-awesome       helvetica-neue-lt-std  hermit                   ibm-plex
    inconsolata        iosevka                league-of-moveable-type  liberation_ttf
    libre-baskerville  libre-bodoni           libre-caslon             libre-franklin
    medio              mplus-outline-fonts    national-park-typeface   norwester-font
    penna              route159               seshat
    tenderness         vegur                  vistafonts

    # media
    ncmpcpp
  ];

  services = {
    gpg-agent = {
      enable           = true;
      defaultCacheTtl  = (60 * 60 * 4);
      enableSshSupport = true;
    };

    mpd.enable = true;
  };
}
