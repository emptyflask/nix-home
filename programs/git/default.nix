{ config, lib, pkgs, ... }:

let
  unstable = import <nixos-unstable> {};
in
{
  xdg.configFile."git/gitattributes".source = ./gitattributes;
  xdg.configFile."git/ignore".source = ./ignore;

  xdg.configFile."pass-git-helper/git-pass-mapping.ini".text = ''
    [github.com*]
    target=dev/github
  '';

  programs.git = {
    enable = true;

    userName = "Jon Roberts";
    userEmail = "jon@emptyflask.net";

    # signing = {
    #   key = "6A4334234032977E";
    #   signByDefault = true;
    # };

    aliases = {
      aliases         = "!git config --get-regexp 'alias.*' | colrm 1 6 | sed 's/[ ]/ = /' | sort";
      st              = "status";
      ci              = "commit";
      co              = "checkout";
      br              = "branch";
      di              = "diff";
      dc              = "diff --cached";
      cp              = "cherry-pick";
      amend           = "commit --amend";
      undo            = "reset --soft HEAD^";
      oneline         = "log --pretty=oneline";
      staged          = "diff --cached";
      unstaged        = "diff";
      recent          = "log --pretty=format:'%Cred%h %Creset- %Cgreen%an (%cd)%Creset: %s' --since='2 weeks ago' --date=short --author=Jon --all";
      tree            = "log --graph --pretty=oneline --abbrev-commit";
      ignore          = "update-index --assume-unchanged";
      parent          = "name-rev --refs='refs/remotes/*' HEAD";
      l               = "log --graph --abbrev-commit --date=relative --pretty=format:'%C(yellow)%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset'";
      la              = "!git l --all";
      head            = "!git l -1";
      h               = "!git head";
      r               = "!git l -30";
      ra              = "!git r --all";
      delete-merged   = ''!git branch --merged | egrep -v "\*|^\s+develop$|^\s+master$" | xargs -n 1 git branch -d'';
      delete-squashed = ''!git checkout -q master && git for-each-ref refs/heads/ "--format=%(refname:short)" | while read branch; do mergeBase=$(git merge-base master $branch) && [[ $(git cherry master $(git commit-tree $(git rev-parse $branch\^{tree}) -p $mergeBase -m _)) == "-"* ]] && git branch -D $branch; done'';
    };

    # Large File Storage
    lfs.enable = true;

    extraConfig = {
      color = {
        ui = "true";

        branch = {
          current = "yellow reverse";
          local   = "yellow";
          remote  = "green";
        };
      };

      core = {
        editor         = "nvim";
        attributesFile = "~/.git/gitattributes";
        pager          =  "${unstable.delta}/bin/delta";
      };

      delta = {
        navigate = true;
        # features = "collared-trogon";
        line-numbers = true;
        syntax-theme = "gruvbox-dark";
        blame-palette = "#1d2021 #282828 #3c3836";
      };

      diff = {
        colorMoved = "default";
        tool       = "nvim";
        hex  = {
          textconv      = "${pkgs.util-linux}/bin/hexdump -v -C";
          binary        = true;
        };
        jpg  = {
          textconv      = "${pkgs.exif}/bin/exif";
          cachetextconv = true;
        };
      };

      # Delta themes
      include.path = "${./themes.gitconfig}";

      interactive = {
        diffFilter = "${unstable.delta}/bin/delta --color-only";
      };

      merge = {
        tool                  = "nvim";
        conflictstyle         = "diff3";
      };

      mergetool.nvim.cmd    = ''nvim -f -c "Gdiffsplit!" "$MERGED"'';

      credential.helper = "${pkgs.gitAndTools.pass-git-helper}/bin/pass-git-helper";
      github.user       = "emptyflask";
      ghi.token         = "!${pkgs.pass}/bin/pass api.github.com | ${pkgs.coreutils}/bin/head -1";

      pull.ff        = "only";
      push.default   = "upstream";
      rerere.enabled = true; # reuse recorded resolution

    };
  };
}
