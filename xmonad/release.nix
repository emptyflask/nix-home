let
  pkgs = import <nixpkgs> {};
in
  pkgs.haskellPackages.callPackage ./build.nix {}
