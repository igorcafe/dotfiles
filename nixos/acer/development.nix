{ pkgs, ... }:
{

  environment.systemPackages = with pkgs; [
    ## VCSs
    git

    ## editors
    vim
    vscode

    ## compilers, LSPs and other tools
    go
    gopls
    python3
    pyright
    python312Packages.pip
    python312Packages.debugpy
    deno
    nodejs
    vue-language-server
    clang-tools
    nixd
    # nodePackages.typescript-language-server
    # gcc
    # pkg-config
    # cmake
    # gnumake
    ghc
    haskell-language-server
    cargo
    rustc
    rust-analyzer
    tokei

    ## dependency management
    # asdf-vm
    direnv

    ## databases and tools
    # mariadb
    # beekeeper-studio
    dbeaver-bin
    (sqlite.override {
      interactive = true;
    })

    ## mobile tools
    # android-studio
    # flutter
    # jdk8

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
    package = (pkgs.emacs-gtk.pkgs.withPackages (epkgs: with epkgs; [
      vterm
      # melpaPackages.telega
    ]));
  };

  # services.ollama = {
  #   enable = true;
  #   package = unstable.ollama;
  # };

  services.redis.servers."".enable = false;
}
