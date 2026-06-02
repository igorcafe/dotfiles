{ pkgs, ... }:
{

  environment.systemPackages = with pkgs; [
    ## VCSs
    git

    ## editors
    vim
    neovim
    # vscode

    ## compilers, LSPs and other tools
    go
    gopls
    python3
    pyright
    # python312Packages.pip
    # # python312Packages.debugpy
    deno
    # nodejs
    # vue-language-server
    clang-tools
    # nixd
    # nodePackages.typescript-language-server
    gcc
    # pkg-config
    # cmake
    # gnumake
    ghc
    haskell-language-server
    # cargo
    # rustc
    # rust-analyzer
    tokei

    ## dependency management
    # asdf-vm
    direnv

    ## databases and tools
    # mariadb
    # beekeeper-studio
    # dbeaver-bin
    (sqlite.override {
      interactive = true;
    })

    bubblewrap

    distrobox

    ## mobile tools
    # android-studio
    # flutter
    # jdk17
    # gradle
    # (pkgs.androidenv.composeAndroidPackages {
    #   platformVersions = [ "34" "35" "36" ];
    #   buildToolsVersions = [ "28.0.3" "34.0.0" "35.0.0" ];
    #   includeEmulator = false;
    #   includeNDK = true;
    #   ndkVersions = [ "26.3.11579264" "28.2.13676358" ];
    #   cmakeVersions = [ "3.22.1" ];
    #   includeSources = false;
    #   includeSystemImages = false;
    #   abiVersions = [ "arm64-v8a" "x86_64" ];
    #   extraLicenses = [
    #     "android-googletv-license"
    #     "android-sdk-arm-dbt-license"
    #     "android-sdk-license"
    #     "android-sdk-preview-license"
    #     "google-gdk-license"
    #     "intel-android-extra-license"
    #     "intel-android-sysimage-license"
    #     "mips-android-sysimage-license"
    #   ];
    # }).androidsdk

    # gcc

    gdb

    binutils

    ## game dev
    godot_4

    # AI coding tools
    # aider-chat-with-playwright
    opencode
  ];

  virtualisation.docker = {
    enable = true;
    liveRestore = false; # required for docker swarm
  };

  # nixpkgs.overlays = [
  #   (import (builtins.fetchTarball {
  #     url = https://github.com/ipvych/telega-overlay/archive/main.tar.gz;
  #   }))
  # ];

  services.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
    defaultEditor = true;
    startWithGraphical = true;
  };

  environment.shellAliases = {
    vim = "nvim";
    ll = "ls -lha";
  };

  services.ollama = {
    enable = false;
  };

  services.redis.servers."".enable = false;
}
