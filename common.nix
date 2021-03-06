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
    nixops
    pandoc              # document converter
    ranger              # CLI file manager
    ripgrep
    shared_mime_info    # recognize file types
    tealdeer            # tldr for various shell tools
    tmux
    translate-shell
    units
    xarchiver

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
