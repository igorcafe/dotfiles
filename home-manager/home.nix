{ inputs, lib, config, pkgs, ... }: {
  imports = [ ./vscode.nix ];
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

  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    enableCompletion = true;
    shellAliases = {
      updatesys = "sudo nixos-rebuild switch --flake ~/Git/dotfiles";
      updatehome = "home-manager switch -b backup --flake ~/Git/dotfiles";
      ffmpeg = "ffmpeg -hide_banner -loglevel error -stats";
      ffplay = "ffplay -hide_banner";
      ffprobe = "ffprobe -hide_banner";
      ls = "lsd";
      ll = "lsd -l";
      lla = "lsd -la";
    };
    zplug = {
      enable = true;
      plugins = [
        {
          name = "plugins/git";
          tags = [ "from:oh-my-zsh" ];
        }
        {
          name = "plugins/colored-man-pages";
          tags = [ "from:oh-my-zsh" ];
        }
        {
          name = "plugins/sudo";
          tags = [ "from:oh-my-zsh" ];
        }
        {
          name = "themes/robbyrussell";
          tags = [ "from:oh-my-zsh" ];
        }
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

    function gcl2() {
      ownerrepo=$(echo "$1" | sed -E 's/((https?:\/\/)|(git@))[^:/]+.//;s/\.git$//;s/([^/]+)\/([^/]+).*/\1\/\2/')
      onwer=$(echo $ownerrepo | cut -d'/' -f1)
      repo=$(echo $ownerrepo | cut -d'/' -f2)
      dest=~/git/$owner/$repo
      mkdir -p $dest
      git clone --recurse-submodules "$1" "$dest"
      cd "$dest"
    }
    '';
  };
}
