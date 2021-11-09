{ pkgs, ... }:

with pkgs;
{
  nvim-cmp = vimUtils.buildVimPlugin {
    name = "nvim-cmp";
    src = fetchFromGitHub {
      owner = "hrsh7th";
      repo = "nvim-cmp";
      rev = "453a62f88208def57de0c0a077d083070fdf7f65";
      sha256 = "1x0415spl6275dr2y7pcqwqn63qin1h6md8jh4snqznphdcgi8p7";
    };
    buildInputs = [stylua luaPackages.luacheck];
  };

  # TODO: add cmp-cmdline
}
