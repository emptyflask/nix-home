{ pkgs, ... }:

with pkgs;
{
  home.packages = with pkgs; [
    cachix
    coreutils

    bat                 # cat clone with syntax highlighting and git integration
    bc                  # cli calculator
    fd                  # find entries in filesystem
    fortune
    htop
    jq
    killall
    kitty               # terminal
    magic-wormhole      # simple secure file transfer
    mosh                # ssh alternative
    nix-index
    nix-prefetch-git
    nix-zsh-completions
    nixops
    pandoc              # document converter
    ranger              # CLI file manager
    ripgrep
    shared_mime_info    # recognize file types
    tmux
    translate-shell
    units
    xarchiver

    # graphics / print
    imagemagick

    # programming - general
    # aws-sam-cli         # AWS serverless app model
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
    vscode
    nixfmt     # format nix
    uncrustify # format c/c++/c#/java/etc

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
    haskellPackages.stylish-haskell
    ormolu
    stack

    # programming - python
    python3Packages.pynvim # for neovim

    # programming - ruby
    bundix
    chruby
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
    fira               fira-code              fira-code-symbols        fira-mono
    font-awesome

    # media
    ncmpcpp
  ];

  services = {
  };
}
