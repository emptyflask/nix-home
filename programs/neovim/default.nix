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
    # package   = neovim-unwrapped;

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
      solargraph
      sumneko-lua-language-server
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
      { plugin = leap-nvim;
        type = "lua";
        config = "require('leap').add_default_mappings()";
      }
      neoformat
      nvim-jdtls
      { plugin = (nvim-treesitter.withPlugins (plugins: with plugins; [
          tree-sitter-bash
          tree-sitter-c
          tree-sitter-c-sharp
          tree-sitter-clojure
          tree-sitter-cmake
          tree-sitter-comment
          tree-sitter-commonlisp
          tree-sitter-cpp
          tree-sitter-css
          tree-sitter-dart
          tree-sitter-dockerfile
          tree-sitter-elixir
          tree-sitter-elm
          tree-sitter-embedded-template
          tree-sitter-erlang
          tree-sitter-fennel
          tree-sitter-go
          tree-sitter-graphql
          tree-sitter-haskell
          tree-sitter-hcl
          tree-sitter-html
          tree-sitter-http
          tree-sitter-java
          tree-sitter-javascript
          tree-sitter-jsdoc
          tree-sitter-json
          tree-sitter-jsonnet
          tree-sitter-kotlin
          tree-sitter-latex
          tree-sitter-llvm
          tree-sitter-lua
          tree-sitter-make
          tree-sitter-markdown
          tree-sitter-nix
          tree-sitter-ocaml
          tree-sitter-ocaml-interface
          tree-sitter-perl
          tree-sitter-php
          tree-sitter-python
          tree-sitter-query
          tree-sitter-regex
          tree-sitter-ruby
          tree-sitter-rust
          tree-sitter-scala
          tree-sitter-scheme
          tree-sitter-scss
          # tree-sitter-sql
          tree-sitter-toml
          tree-sitter-tsx
          tree-sitter-typescript
          tree-sitter-vim
          tree-sitter-yaml
          tree-sitter-zig
        ]));
        type = "lua";
        config = builtins.readFile(./treesitter.lua);
      }
      {
        plugin = (pluginGit "master" "ckolkey/ts-node-action");
        type = "lua";
        config = ''
          require("ts-node-action").setup({})
          vim.keymap.set({ "n" }, "<F12>", require("ts-node-action").node_action, { desc = "Trigger Node Action" })
        '';
      }
      { plugin = nvim-lspconfig;
        type = "lua";
        config = builtins.readFile(./lspconfig.lua);
      }
      { plugin = (pluginGit "main" "echasnovski/mini.nvim");
        type = "lua";
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
        plugin = (pluginGit "main" "ellisonleao/gruvbox.nvim");
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
      # { plugin = pluginGit "main" "semanticart/ruby-code-actions.nvim";
      #   type = "lua";
      #   config = builtins.readFile(./ruby-code-actions.lua);
      # }
      { plugin = rust-tools-nvim;
        type = "lua";
        config = builtins.readFile(./rust-tools.lua);
      }
      vim-stylish-haskell
      vim-polyglot  # syntax highlighting for most languages
      vim-rails
      vim-terraform
      vimwiki

      (pluginGit "master" "rescript-lang/vim-rescript")

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
      { plugin = vim-vsnip;
        config = builtins.readFile(./vsnip.vim);
      }
      vim-vsnip-integ
      vim-snippets
    ];

  };

  # all ftplugin configs:
  xdg.configFile."nvim/ftplugin/" = {
    source = ./ftplugin;
    recursive = true;
  };
}
