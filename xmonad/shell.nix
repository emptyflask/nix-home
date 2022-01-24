{ nixpkgs ? import <nixpkgs> {} }:

let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  project = import ./release.nix;

  shell-pkgs = with haskellPackages; [
    implicit-hie
  ];

in
  pkgs.stdenv.mkDerivation {
    name = "shell";
    buildInputs = shell-pkgs ++ project.env.nativeBuildInputs;
  }
