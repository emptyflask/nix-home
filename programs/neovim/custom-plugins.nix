{ pkgs, ... }:

with pkgs;
{
  nvim-cmp = vimUtils.buildVimPlugin {
    name = "nvim-cmp";
    src = fetchFromGitHub {
      owner = "hrsh7th";
      repo = "nvim-cmp";
      rev = "da4c071f6bdb1edfa1debc7e1f7842c2d620bb04";
      sha256 = "03bjnrdm4aw4a50agapdz0iqpsb2xhx92pwkdl7p3rh8vjbggndb";
    };
    buildInputs = [stylua luaPackages.luacheck];
  };

  cmp-buffer = vimUtils.buildVimPlugin {
    name = "cmp-buffer";
    src = fetchFromGitHub {
      owner = "hrsh7th";
      repo = "cmp-buffer";
      rev = "a46e1ce308c86123dfa21f5934cef4e8042171f5";
      sha256 = "0a8lkbsv0wkbffbhn8gmhnfq9hgr8c77myxq243v04wm739c775y";
    };
  };

  cmp-cmdline = vimUtils.buildVimPlugin {
    name = "cmp-cmdline";
    src = fetchFromGitHub {
      owner = "hrsh7th";
      repo = "cmp-cmdline";
      rev = "0ca73c3a50b72c2ca168d8904b39aba34d0c4227";
      sha256 = "1777rv9mh3bar8lp5i4af7kip5j3s4ib8a83b67clga8pcdjla4d";
    };
  };

#   fugitive = vimUtils.buildVimPlugin {
#     name = "fugitive";
#     src = fetchFromGitHub {
#       owner = "tpope";
#       repo = "vim-fugitive";
#       rev = "2064312ad7bb80050baf9acbdfb7641162919e53";
#       sha256 = "1mjhvbr1ngh85ab306kjl44bd6caq04vp0bbwgkmvpc6c3r1b2d3";
#     };
#   };
}
