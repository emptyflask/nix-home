{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

let

  ImageBase = perlPackages.buildPerlPackage {
    pname = "Image-Base";
    version = "1.17";
    src = fetchurl {
      url = "mirror://cpan/authors/id/K/KR/KRYDE/Image-Base-1.17.tar.gz";
      sha256 = "f6d0d4d03026ba6a19d2ac3495171fc123522345630cadc7f43b53a667b95f81";
    };
    meta = {
      description = "Base class for image manipulation";
    };
  };

  ImageXpm = perlPackages.buildPerlPackage {
    pname = "Image-Xpm";
    version = "1.13";
    src = fetchurl {
      url = "mirror://cpan/authors/id/S/SR/SREZIC/Image-Xpm-1.13.tar.gz";
      sha256 = "55da78fccf4c19d3d173fab38fc6ce6df0078f839a8a3e699199e4ef19428803";
    };
    propagatedBuildInputs = [ ImageBase ];
    meta = {
      description = "Load, create, manipulate and save xpm image files";
      license = stdenv.lib.licenses.gpl2Plus;
    };
  };

in stdenv.mkDerivation {
  name        = "xmobar-icons";
  version     = "0.0.1";

  buildInputs = [
    perl
    ImageXpm
  ];
}
