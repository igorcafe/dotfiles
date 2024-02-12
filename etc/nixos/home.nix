{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  nixpkgs = {

    overlays = [
      (final: prev: {
        postman = prev.postman.overrideAttrs(old: rec {
          version = "20230716100528";
          src = final.fetchurl {
            url = "https://web.archive.org/web/${version}/https://dl.pstmn.io/download/latest/linux_64";
            sha256 = "sha256-svk60K4pZh0qRdx9+5OUTu0xgGXMhqvQTGTcmqBOMq8=";

            name = "${old.pname}-${version}.tar.gz";
          };
        });
      })
    ];

    config = {
      allowUnfree = true;
      allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [ "steam" "steam-original" "steam-run" ];
    };
  };

  systemd.user.startServices = "sd-switch";

  home = {
    username = "user";
    homeDirectory = "/home/user";
    stateVersion = "23.11";

    packages = with pkgs; [
      firefox
      kate
      google-chrome
      telegram-desktop
      gnome.cheese
      vscode
      kcalc
      signal-desktop
      gcc
      go
      vlc
      gimp
      ffmpeg_6-full
      obs-studio
      spectacle
      kdenlive
      unzip
      stremio
      insomnia
      lsd
      fzf
      zoxide
      retroarchFull
      audacity
      godot3
      golangci-lint
      htop
      musescore
      nodejs
      p7zip
      python3
      qbittorrent
      tealdeer
      unrar
      nixfmt
      sqlite
      appimage-run
      calibre
      qpwgraph
      rlwrap
      xonotic
      jq
      postman
    ];
  };

  programs.git.enable = true;
  programs.home-manager.enable = true;

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    shellAliases = {
      update = "sudo nixos-rebuild switch";
      upgrade = "sudo nixos-rebuild --upgrade boot";
      ffmpeg = "ffmpeg -hide_banner -loglevel error -stats";
      ffplay = "ffplay -hide_banner";
      ffprobe = "ffprobe -hide_banner";
    };
    zplug = {
      enable = true;
      plugins = [
        { name = "plugins/git"; tags = [ from:oh-my-zsh ]; }
        { name = "plugins/colored-man-pages"; tags = [ from:oh-my-zsh ]; }
        { name = "plugins/sudo"; tags = [ from:oh-my-zsh ]; }
        { name = "themes/robbyrussell"; tags = [ from:oh-my-zsh ]; }
        { name = "zsh-users/zsh-autosuggestions"; }
        { name = "zsh-users/zsh-syntax-highlighting"; }
      ];
    };
    initExtra = ''
    bindkey "^[[1;5C" forward-word
    bindkey "^[[1;5D" backward-word

    gotemp () {
      d=/tmp/gotemp-$RANDOM
      mkdir $d && cd $d && echo "package main\n\nfunc main() {\n\t\n}\n" > main.go
    }
    '';
  };
}
