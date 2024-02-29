{ inputs, lib, config, pkgs, ... }: {
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
      rnix-lsp
      google-cloud-sdk
    ];
  };

  programs.google-chrome = {
    enable = true;
    # commandLineArgs = [ "--force-device-scale-factor=1.0" ];
  };

  programs.vscode = {
    enable = true;
    enableUpdateCheck = false;
    extensions = with pkgs.vscode-extensions; [
      jnoortheen.nix-ide
      vscodevim.vim
      eamodio.gitlens
      golang.go
    ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "reload";
        publisher = "natqe";
        version = "0.0.6";
        sha256 = "bTFLk3sCJb7ztkC/Cxci6n7RbcyNjEYNKREUf9wDQRU=";
      }
      {
        name = "vscode-go-template";
        publisher = "jinliming2";
        version = "0.2.1";
        sha256 = "kvWaSuE4O98fXU+EKkplFI0Cfy2I7IiJ0/hXrVsk+6g=";
      }
      {
        name = "ayu";
        publisher = "teabyii";
        version = "1.0.5";
        sha256 = "+IFqgWliKr+qjBLmQlzF44XNbN7Br5a119v9WAnZOu4=";
      }
      {
        name = "material-icon-theme";
        publisher = "PKief";
        version = "4.33.0";
        sha256 = "Rwpc5p7FOSodGa1WWrjgkexzAp8RlgZCYBXhep1G5Pk=";
      }
    ];
    keybindings = [
      {
        key = "ctrl+shift+c";
        command = "editor.action.clipboardCopyAction";
        when = "textInputFocus";
      }
      {
        key = "alt+d";
        command = "editor.action.addSelectionToNextFindMatch";
        when = "editorFocus";
      }
      {
        key = "ctrl+d";
        command = "-editor.action.addSelectionToNextFindMatch";
        when = "editorFocus";
      }
      {
        key = "alt+r";
        command = "workbench.action.openRecent";
      }
      {
        key = "ctrl+r";
        command = "-workbench.action.openRecent";
      }
      {
        key = "ctrl+shift+v";
        command = "-markdown.showPreview";
        when = "!notebookEditorFocused && editorLangId == 'markdown'";
      }
      {
        key = "alt+f";
        command = "actions.find";
        when = "editorFocus || editorIsOpen";
      }
      {
        key = "ctrl+f";
        command = "-actions.find";
        when = "editorFocus || editorIsOpen";
      }
      {
        key = "ctrl+k o";
        command = "workbench.action.closeOtherEditors";
      }
      {
        key = "ctrl+k o";
        command = "-workbench.action.files.showOpenedFileInNewWindow";
        when = "emptyWorkspaceSupport";
      }
      {
        key = "ctrl+k ctrl+alt+s";
        command = "git.stageSelectedRanges";
        when = "!operationInProgress";
      }
      {
        key = "ctrl+k ctrl+alt+s";
        command = "-git.stageSelectedRanges";
        when = "isInDiffEditor && !operationInProgress";
      }
    ];

    userSettings = {
      # themes
      "workbench.colorTheme" = "Default Light Modern";
      "workbench.iconTheme" = "material-icon-theme";

      # vim
      "vim.handleKeys" = {
        "<C-n>" = false;
        "<C-p>" = false;
        "<C-b>" = false;
        "<C-k>" = false;
      };
      "vim.normalModeKeyBindings" = [
        {
          "before" = [ "g" "i" ];
          "commands" = [ "editor.action.goToImplementation" ];
        }
      ];
      "vim.smartRelativeLine" = true;

      # go
      "go.lintTool" = "golangci-lint";
      "go.testTimeout" = "5s";

      #nix
      "nix.enableLanguageServer" = true;
      "[nix]" = {
        "editor.defaultFormatter" = "jnoortheen.nix-ide";
        "editor.formatOnSave" = true;
      };

      "files.trimTrailingWhitespace" = true;
      # "html.autoClosingTags" = false;
      "editor.minimap.enabled" = false;
      "git.suggestSmartCommit" = false;
      "window.zoomLevel" = 1;
      "git.confirmSync" = false;
    };
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
    enableAutosuggestions = true;
    enableCompletion = true;
    shellAliases = {
      updatesys = "sudo nixos-rebuild switch --flake ~/Git/dotfiles";
      updatehome = "home-manager switch -b backup --flake ~/Git/dotfiles";
      ffmpeg = "ffmpeg -hide_banner -loglevel error -stats";
      ffplay = "ffplay -hide_banner";
      ffprobe = "ffprobe -hide_banner";
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
    '';
  };
}
