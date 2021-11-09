{pkgs, ...}:

let
  unstable = import <nixos-unstable> {};
  plugins = pkgs.vimPlugins // pkgs.callPackage ./custom-plugins.nix {pkgs = unstable;};

  concatFiles =
    files:
    let
      wrapLua = contents: "lua << EOF\n" + contents + "\nEOF\n";
      read = file: if pkgs.lib.hasSuffix ".lua" file then wrapLua (builtins.readFile file) else builtins.readFile file;
    in
      pkgs.lib.strings.concatMapStringsSep "\n" read files;
in

with pkgs;

{
  programs.neovim = {
    enable    = true;
    package   = unstable.neovim-unwrapped;
    # package   = neovim-nightly;

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
    ]) + ''
      let g:dictionary = "${scowl}/share/dict/words.txt"

      function! UUID()
        return system('${util-linux}/bin/uuidgen')[0:-2]
      endfunction
    '';

    plugins = with plugins; [
      Hoogle
      Rename
      Tabular
      Tagbar
      unstable.vimPlugins.cmp-buffer
      unstable.vimPlugins.cmp-nvim-lsp
      unstable.vimPlugins.cmp-path
      unstable.vimPlugins.cmp-vsnip
      unstable.vimPlugins.vim-vsnip
      # coc-eslint
      # coc-fzf
      # coc-java
      # coc-json
      # { plugin = coc-nvim;
      #   config = ''let g:coc_data_home = "~/.config/coc"'';
      # }
      # coc-pairs
      # coc-prettier
      # coc-snippets
      # coc-solargraph
      editorconfig-vim
      elm-vim
      fastfold
      fugitive
      fzf-vim
      fzfWrapper
      ghc-mod-vim
      { plugin = gitgutter;
        config = ''let g:gitgutter_git_executable = "${git}/bin/git"'';
      }
      gruvbox-community
      haskell-vim
      # hlint-refactor
      # intero-neovim
      lightline-vim
      # neco-ghc
      neoformat
      nvim-cmp
      nvim-jdtls
      nvim-tree-lua
      # nvim-treesitter
      (nvim-treesitter.withPlugins (plugins: pkgs.tree-sitter.allGrammars))
      nvim-lspconfig
      nvim-web-devicons
      repeat
      sensible
      tlib
      undotree
      vim-abolish
      vim-commentary
      vim-dispatch
      vim-grepper
      vim-gutentags
      vim-polyglot  # syntax highlighting for most languages
      vim-sandwich
      vim-snippets
      vim-startify
      vim-stylish-haskell
      vim-test
      vim-tmux-navigator
      vim-unimpaired
      vimproc
      vimwiki
    ];

  };

  # xdg.configFile."nvim/coc-settings.json".source = ./coc-settings.json;
  xdg.configFile."nvim/ftplugin/ruby.vim".source = ./ftplugin/ruby.vim;
  xdg.configFile."nvim/after/plugin/tabular.vim".source = ./after/plugin/tabular.vim;
}
