{ config, pkgs, lib, ... }:

let
  concatFiles = files:
    pkgs.lib.strings.concatMapStringsSep "\n" builtins.readFile files;

  # installs a vim plugin from git with a given tag / branch
  # usage: pluginGit "HEAD" "ellisonleao/gruvbox.nvim");
  pluginGit = ref: repo: pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "${lib.strings.sanitizeDerivationName repo}";
    version = ref;
    src = builtins.fetchGit {
      url = "https://github.com/${repo}.git";
      ref = ref;
    };
  };

in

with pkgs;

{
  programs.neovim = {
    enable    = true;
    package   = neovim-unwrapped;

    viAlias   = true;
    vimAlias  = false;

    withNodeJs = true;

    extraConfig = (concatFiles [
      ./config.vim
      ./haskell.vim
      ./keymap.vim
      ./netrw.vim
      ./rename.vim
      ./status.vim
      ./theme.vim
    ]) + ''
      let g:dictionary = "${scowl}/share/dict/words.txt"

      function! UUID()
        return system('${util-linux}/bin/uuidgen')[0:-2]
      endfunction
    '';

    extraPackages = [
      dhall-lsp-server
      nodePackages.typescript
      nodePackages.typescript-language-server
      rust-analyzer
      shfmt
      terraform-lsp
      tree-sitter
    ];

    plugins = with pkgs.vimPlugins; [
      Rename
      { plugin = Tabular;
        runtime = { "after/plugin/tabular.vim".source = ./after/plugin/tabular.vim; };
      }
      Tagbar
      editorconfig-vim
      fugitive
      neoformat
      nvim-jdtls
      { plugin = (nvim-treesitter.withPlugins (plugins: pkgs.tree-sitter.allGrammars));
        type = "lua";
        config = builtins.readFile(./treesitter.lua);
      }
      { plugin = nvim-lspconfig;
        type = "lua";
        config = builtins.readFile(./lspconfig.lua);
      }
      repeat
      sensible
      tlib
      undotree
      vim-abolish
      vim-commentary
      vim-dispatch
      vim-grepper
      vim-gutentags
      vim-sandwich
      vim-signify
      vim-startify
      vim-test
      vim-tmux-navigator
      vim-unimpaired
      vimproc

      # THEME / VISUAL
      lightline-vim
      { # Lua port of gruvbox-community w/ treesitter support
        plugin = (pluginGit "HEAD" "ellisonleao/gruvbox.nvim");
        type = "lua";
        config = ''
          require("gruvbox").setup({ contrast = "hard" })
        '';
      }
      kanagawa-nvim
      { plugin = onedark-nvim;
        type = "lua";
        config = ''
          require('onedark').setup { style = 'warmer' }
        '';
      }
      tokyonight-nvim
      nvim-ts-rainbow  # TS multicolored parens/brackets

      # FILE EXPLORER
      { plugin = nvim-tree-lua;
        type = "lua";
        config = builtins.readFile(./nvim-tree.lua);
      }
      nvim-web-devicons
      fzf-vim
      fzfWrapper
      plenary-nvim
      { plugin = telescope-nvim;
        type = "lua";
        config = builtins.readFile(./telescope.lua);
      }
      telescope-fzf-native-nvim

      # LANGUAGE / FILETYPE SPECIFIC
      Hoogle
      dhall-vim
      elm-vim
      ghc-mod-vim
      haskell-vim
      neco-ghc
      hlint-refactor
      # intero-neovim
      vim-stylish-haskell
      vim-polyglot  # syntax highlighting for most languages
      vim-rails
      vim-terraform
      vimwiki

      # COMPLETION
      cmp-buffer
      cmp-cmdline
      cmp-cmdline-history
      { plugin = nvim-cmp;
        type = "lua";
        config = builtins.readFile(./nvim-cmp.lua);
      }
      cmp-nvim-lsp
      cmp-nvim-lua
      cmp-path
      cmp-vsnip
      vim-vsnip
      vim-snippets
    ];

  };

  # all ftplugin configs:
  xdg.configFile."nvim/ftplugin/" = {
    source = ./ftplugin;
    recursive = true;
  };
}
