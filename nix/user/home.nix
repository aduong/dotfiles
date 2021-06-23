{ pkgs, ... }:

{
  fonts.fontconfig.enable = true;

  home.file = {
    bash_aliases = {
      source = ~/workspace/dotfiles/bash_aliases;
      target = ".bash_aliases";
    };

    dotspacemacs = {
      source = ~/workspace/dotfiles/spacemacs;
      target = ".spacemacs";
    };

    gitconfig = {
      source = ~/workspace/dotfiles/gitconfig;
      target = ".gitconfig";
    };

    spacemacs = {
      recursive = true;
      source = pkgs.fetchFromGitHub {
        owner = "syl20bnr";
        repo = "spacemacs";
        rev = "develop";
        sha256 = "01f9cb3iqlz5x0wdgymaxkqrsl0hbws5x0kbj52mfpqcid3s2dl0";
      };
      target = ".emacs.d";
    };

    starship = {
      source = ~/workspace/dotfiles/starship.toml;
      target = ".config/starship.toml";
    };

  };

  home.packages = with pkgs; [
    adapta-gtk-theme
    bitwarden
    elementary-xfce-icon-theme
    evince
    calibre
    fontconfig
    git
    jq
    htop
    libnotify
    ncdu
    (nerdfonts.override { fonts = [ "DejaVuSansMono" "FiraCode" ]; })
    restic
    ripgrep
    signal-desktop
    tree
    vlc
    xsel
  ];

  services = {
    emacs.enable = true;

    redshift = {
      enable = true;
      provider = "geoclue2";
    };

    syncthing = {
      enable = true;
      tray = true;
    };
  };

  programs =  {
    autojump.enable = true;

    bash = {
      enable = true;
      historyControl = [ "erasedups" ];
      initExtra = ". ~/.bash_aliases";
    };

    dircolors.enable = true;
    emacs.enable = true;
    starship.enable = true;
  };
}

