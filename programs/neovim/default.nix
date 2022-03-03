{ config, pkgs, lib, ... }:

let
  unstable = (if pkgs.stdenv.isDarwin then (import <nixpkgs>) else (import <nixos-unstable>)) {};
  # custom = pkgs.vimPlugins // pkgs.callPackage ./custom-plugins.nix {pkgs = unstable;};

  concatFiles = files:
    let
      wrapLua = contents: "lua << EOF\n" + contents + "\nEOF\n";

      read = file:
        if pkgs.lib.hasSuffix ".lua" file then
          wrapLua (builtins.readFile file)
        else
          builtins.readFile file;
    in
      pkgs.lib.strings.concatMapStringsSep "\n" read files;

  # installs a vim plugin from git with a given tag / branch
  pluginGit = ref: repo: pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "${lib.strings.sanitizeDerivationName repo}";
    version = ref;
    src = builtins.fetchGit {
      url = "https://github.com/${repo}.git";
      ref = ref;
    };
  };

  # always installs latest version
  plugin = pluginGit "HEAD";

in

with pkgs;

{
  programs.neovim = {
    enable    = true;
    package   = unstable.neovim-unwrapped;

    viAlias   = true;
    vimAlias  = false;

    withNodeJs = true;

    extraConfig = (concatFiles [
      ./config.vim
      # ./coc.vim
      ./haskell.vim
      ./keymap.vim
      ./netrw.vim
      ./rename.vim
      ./status.vim
      ./theme.vim
      ./nvim-cmp.lua
      ./lspconfig.lua
      ./telescope.lua
      ./treesitter.lua
      ./nvim-tree.lua
    ]) + ''
      let g:dictionary = "${scowl}/share/dict/words.txt"

      function! UUID()
        return system('${util-linux}/bin/uuidgen')[0:-2]
      endfunction
    '';

    extraPackages = [
      nodePackages.typescript
      nodePackages.typescript-language-server
      rust-analyzer
      shfmt
      tree-sitter
    ];

    plugins = with pkgs.vimPlugins; [
      Rename
      Tabular
      Tagbar
      editorconfig-vim
      unstable.vimPlugins.fugitive
      # { plugin = gitgutter;
      #   config = ''let g:gitgutter_git_executable = "${git}/bin/git"'';
      # }
      neoformat
      nvim-jdtls
      (nvim-treesitter.withPlugins (plugins: pkgs.tree-sitter.allGrammars))
      nvim-lspconfig
      repeat
      sensible
      unstable.vimPlugins.tlib
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
      # (plugin "rktjmp/lush.nvim")  # TS theming lib
      # (plugin "ellisonleao/gruvbox.nvim")  # TS port of gruvbox
      (plugin "rebelot/kanagawa.nvim") # alternate colorscheme
      (plugin "folke/tokyonight.nvim") # another alternate colorscheme
      gruvbox-community
      # nvim-ts-rainbow  # TS multicolored parens/brackets

      # FILE EXPLORER
      nvim-tree-lua
      nvim-web-devicons
      # (plugin "kyazdani42/nvim-web-devicons")
      # (plugin "kyazdani42/nvim-tree.lua")
      fzf-vim
      fzfWrapper
      plenary-nvim
      telescope-nvim
      telescope-fzf-native-nvim

      # LANGUAGE / FILETYPE SPECIFIC
      Hoogle
      elm-vim
      ghc-mod-vim
      haskell-vim
      # neco-ghc
      hlint-refactor
      # intero-neovim
      vim-stylish-haskell
      vim-polyglot  # syntax highlighting for most languages
      vim-rails
      vimwiki

      # COMPLETION
      (plugin "hrsh7th/nvim-cmp")
      (plugin "hrsh7th/cmp-buffer")
      (plugin "hrsh7th/cmp-cmdline")
      unstable.vimPlugins.cmp-nvim-lsp
      unstable.vimPlugins.cmp-nvim-lua
      unstable.vimPlugins.cmp-path
      unstable.vimPlugins.cmp-vsnip
      unstable.vimPlugins.vim-vsnip
      vim-snippets
    ];

  };

  xdg.configFile."nvim/ftplugin/" = {
    source = ./ftplugin;
    recursive = true;
  };
  xdg.configFile."nvim/after/plugin/tabular.vim".source = ./after/plugin/tabular.vim;
}
