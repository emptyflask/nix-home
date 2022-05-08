{ pkgs, lib, ... }:

with pkgs;
{

  home.packages = with pkgs; [

    bat                 # cat clone with syntax highlighting and git integration
    bc                  # cli calculator
    fd                  # find entries in filesystem
    fortune
    htop
    jq
    killall
    magic-wormhole      # simple secure file transfer
    mosh                # ssh alternative
    nix-index
    nix-prefetch-git
    nix-zsh-completions
    pandoc              # document converter
    ranger              # CLI file manager
    ripgrep
    shared-mime-info    # recognize file types
    tealdeer            # tldr for various shell tools
    translate-shell
    units
    # xarchiver

    # graphics / print
    imagemagick

    # programming - general
    exercism
    foreman
    gnumake
    html-tidy           # format html
    niv                 # nix channel config
    sourceHighlight
    shellcheck          # shell script analyzer
    tig                 # git tui frontend
    universal-ctags

    # programming - elixir / erlang
    elixir

    # programming - javascript
    nodejs
    nodePackages.diagnostic-languageserver
    nodePackages.eslint_d
    nodePackages.typescript
    nodePackages.typescript-language-server

    # programming - haskell
    ghc
    cabal2nix
    cabal-install
    haskellPackages.apply-refact
    haskellPackages.ghcid
    haskellPackages.haskell-language-server
    haskellPackages.hlint
    haskellPackages.yesod

    # programming - python
    python3Packages.pynvim # for neovim

    # programming - ruby
    bundix
    jekyll
    ruby_3_0
    rubyPackages_3_0.pry

    # programming - rust
    cargo
    rustc
    rustfmt

    # chat / email
    neomutt             # CLI mail
    protonmail-bridge
    weechat

    # fonts
    fira
    fira-code
    fira-code-symbols
    fira-mono
    font-awesome

    # media
    ncmpcpp
  ];

}
