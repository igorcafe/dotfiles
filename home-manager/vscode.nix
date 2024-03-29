{ pkgs, ... }: {
  programs.vscode = {
    enable = true;
    package = pkgs.vscode;
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
      {
        key = "ctrl+shift+alt+o";
        command = "workbench.action.showAllSymbols";
      }
      {
        key = "ctrl+t";
        command = "-workbench.action.showAllSymbols";
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
      "nix.formatterPath" = "nixpkgs-fmt";
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
}