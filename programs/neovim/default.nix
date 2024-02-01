{ pkgs, lib, ... }:

let
  concatFiles = files:
    pkgs.lib.strings.concatMapStringsSep "\n" builtins.readFile files;

in

with pkgs;

{
  programs.neovim = {
    enable    = true;
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
    ]) + ''
      let g:dictionary = "${scowl}/share/dict/words.txt"

      function! UUID()
        return system('${util-linux}/bin/uuidgen')[0:-2]
      endfunction
    '';

    extraPackages = [
      # dhall-lsp-server
      nodePackages.typescript
      nodePackages.typescript-language-server
      rust-analyzer
      shfmt
      solargraph
      sumneko-lua-language-server
      terraform-ls
      tree-sitter
    ];

    plugins = pkgs.callPackage ./plugins.nix {};
  };

  # all ftplugin configs:
  xdg.configFile."nvim/ftplugin/" = {
    source = ./ftplugin;
    recursive = true;
  };
}
