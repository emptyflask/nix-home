{ pkgs, lib, ... }:

let
  # installs a vim plugin from git with a given tag / branch
  # usage: pluginGit "HEAD" "ellisonleao/gruvbox.nvim");
  pluginGit = ref: repo: pkgs.vimUtils.buildVimPlugin {
    pname = "${lib.strings.sanitizeDerivationName repo}";
    version = ref;
    src = builtins.fetchGit {
      url = "https://github.com/${repo}.git";
      ref = ref;
    };
  };

  custom = {
    copilot-cmp = { plugin = pkgs.vimPlugins.copilot-cmp;
      type = "lua";
      config = ''
        require("copilot_cmp").setup()
      '';
    };

    copilot-lua = { plugin = pkgs.vimPlugins.copilot-lua;
      type = "lua";
      config = ''
      require("copilot").setup({
        suggestion  = { enabled = false },
        panel       = { enabled = false },
      })
      '';
    };

    dashboard = {
      plugin = pkgs.vimPlugins.alpha-nvim;
      type = "lua";
      config = builtins.readFile(./dashboard.lua);
    };

    gruvbox = { # Lua port of gruvbox-community w/ treesitter support
      plugin = (pluginGit "main" "ellisonleao/gruvbox.nvim");
      type = "lua";
      config = ''
        require("gruvbox").setup({ contrast = "hard" })
        vim.cmd.colorscheme("gruvbox")
      '';
    };

    leap-nvim = { plugin = pkgs.vimPlugins.leap-nvim;
      type = "lua";
      config = "require('leap').add_default_mappings()";
    };

    lspconfig = { plugin = pkgs.vimPlugins.nvim-lspconfig;
      type = "lua";
      config = builtins.readFile(./lspconfig.lua);
    };

    mini = { plugin = (pluginGit "main" "echasnovski/mini.nvim");
      type = "lua";
      config = ''
        require('mini.ai').setup()
        require('mini.align').setup()
        require('mini.bracketed').setup()
        require('mini.comment').setup({
          mappings = {
            comment = "gc",
            comment_line = "\\\\\\",
            comment_visual = "\\\\",
            textobject = "gc"
          }
        })
        require('mini.move').setup()
        require('mini.pairs').setup()
      '';
    };

    nvim-cmp = { plugin = pkgs.vimPlugins.nvim-cmp;
      type = "lua";
      config = builtins.readFile(./nvim-cmp.lua);
    };

    nvim-tree = { plugin = pkgs.vimPlugins.nvim-tree-lua;
      type = "lua";
      config = builtins.readFile(./nvim-tree.lua);
    };

    onedark = { plugin = pkgs.vimPlugins.onedark-nvim;
      type = "lua";
      config = ''
        require('onedark').setup { style = 'warmer' }
      '';
    };

    ruby-code-actions = { plugin = pluginGit "main" "semanticart/ruby-code-actions.nvim";
      type = "lua";
      config = builtins.readFile(./ruby-code-actions.lua);
    };

    rust-tools = { plugin = pkgs.vimPlugins.rust-tools-nvim;
      type = "lua";
      config = builtins.readFile(./rust-tools.lua);
    };

    tabular = { plugin = pkgs.vimPlugins.Tabular;
      runtime = { "after/plugin/tabular.vim".source = ./after/plugin/tabular.vim; };
    };

    telescope = { plugin = pkgs.vimPlugins.telescope-nvim;
      type = "lua";
      config = builtins.readFile(./telescope.lua);
    };

    treesitter = { plugin = pkgs.vimPlugins.nvim-treesitter.withAllGrammars;
      type = "lua";
      config = builtins.readFile(./treesitter.lua);
    };

    ts-node-action = {
      plugin = (pluginGit "master" "ckolkey/ts-node-action");
      type = "lua";
      config = ''
        require("ts-node-action").setup({})
        vim.keymap.set({ "n" }, "<F12>", require("ts-node-action").node_action, { desc = "Trigger Node Action" })
      '';
    };

    vsnip = { plugin = pkgs.vimPlugins.vim-vsnip;
      config = builtins.readFile(./vsnip.vim);
    };

  };
in



    with pkgs.vimPlugins; [
      Rename
      Tagbar
      custom.dashboard
      custom.leap-nvim
      custom.lspconfig
      custom.mini
      custom.tabular
      custom.treesitter
      custom.ts-node-action
      editorconfig-vim
      fugitive
      gitsigns-nvim
      neoformat
      none-ls-nvim
      nvim-jdtls
      repeat
      sensible
      tlib
      undotree
      vim-abolish
      # vim-commentary
      vim-dispatch
      vim-grepper
      vim-gutentags
      vim-sandwich
      vim-test
      vim-tmux-navigator
      # vim-unimpaired
      vimproc

      # THEME / VISUAL
      custom.gruvbox
      custom.onedark
      lightline-vim
      kanagawa-nvim
      tokyonight-nvim
      rainbow-delimiters-nvim # Treesitter multicolored parens/brackets

      # FILE EXPLORER
      custom.nvim-tree
      custom.telescope
      nvim-web-devicons
      fzf-vim
      fzfWrapper
      plenary-nvim
      telescope-fzf-native-nvim

      # LANGUAGE / FILETYPE SPECIFIC
      Hoogle
      custom.ruby-code-actions
      custom.rust-tools
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

      (pluginGit "master" "rescript-lang/vim-rescript")

      # COMPLETION
      cmp-buffer
      cmp-cmdline
      cmp-cmdline-history
      custom.nvim-cmp
      cmp-nvim-lsp
      cmp-nvim-lua
      cmp-path
      cmp-vsnip
      custom.vsnip
      lspkind-nvim
      vim-vsnip-integ
      vim-snippets

      # COPILOT
      custom.copilot-cmp
      custom.copilot-lua
    ]
