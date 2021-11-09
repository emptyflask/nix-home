{pkgs, ...}:

{
  home.sessionVariables = {
    _JAVA_AWT_WM_NONREPARENTING=1;

    ACK_COLOR_MATCH="red";

    EDITOR   = "nvim";
    LESS     = "-F -R -M -i";
    LESSOPEN = "| ${pkgs.sourceHighlight}/bin/src-hilite-lesspipe.sh %s";
    MANPAGER = "${pkgs.neovim}/bin/nvim +Man!";
    PAGER    = "${pkgs.less}/bin/less";
  };
}
