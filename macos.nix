{ pkgs, ... }:

with pkgs;
{
  home.packages = with pkgs; [
    cachix
    coreutils
    xdg-utils

    # programming - general
    nixfmt     # format nix
    uncrustify # format c/c++/c#/java/etc

    # programming - haskell
    haskellPackages.stylish-haskell
    ormolu
    stack

    # programming - ruby
    chruby
  ];
}
