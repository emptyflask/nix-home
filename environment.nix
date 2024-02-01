{pkgs, ...}:

{
  home.sessionVariables = {
    _JAVA_AWT_WM_NONREPARENTING=1;

    ACK_COLOR_MATCH="red";

    EDITOR   = "nvim";
    LESS     = "-F -R -M -i";
    LESSOPEN = "| ${pkgs.sourceHighlight}/bin/src-hilite-lesspipe.sh %s";
    MANPAGER = "nvim +Man!";
    PAGER    = "${pkgs.less}/bin/less";

    # xz should use all available cores
    XZ_DEFAULTS="-T 0";
  };
}
