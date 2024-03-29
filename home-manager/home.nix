{ lib, pkgs, ... }: {
  imports = [
    ./vscode.nix
    ./zsh.nix
  ];

  nixpkgs = {
    overlays = [
      (final: prev: {
        postman = prev.postman.overrideAttrs (old: rec {
          version = "20230716100528";
          src = final.fetchurl {
            url =
              "https://web.archive.org/web/${version}/https://dl.pstmn.io/download/latest/linux_64";
            sha256 = "sha256-svk60K4pZh0qRdx9+5OUTu0xgGXMhqvQTGTcmqBOMq8=";

            name = "${old.pname}-${version}.tar.gz";
          };
        });
      })
    ];

    config = {
      allowUnfree = true;
      allowUnfreePredicate = pkg:
        builtins.elem (lib.getName pkg) [
          "steam"
          "steam-original"
          "steam-run"
        ];
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
      telegram-desktop
      gnome.cheese
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
      sqlite
      appimage-run
      calibre
      qpwgraph
      rlwrap
      xonotic
      jq
      postman
      nixpkgs-fmt
      pgcli
      yt-dlp
      asdf-vm
      google-cloud-sdk
      flameshot
      ripgrep
      nil
      mangohud
    ];
  };

  programs.google-chrome = {
    enable = true;
    # commandLineArgs = [ "--force-device-scale-factor=1.0" ];
  };


  programs.git = {
    enable = true;
    extraConfig = {
      url = { "git@github.com:" = { insteadOf = "https://github.com/"; }; };
    };
  };

  programs.home-manager.enable = true;
}
