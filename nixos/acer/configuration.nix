# Here remains the config I don't feel like putting in a special file

{ pkgs, ... }:
{
  imports = [
    # games, launchers, graphics, performance tools, custom kernel and power settings
    ./gaming.nix

    # compilers, LSPs, editors, databases, docker, AI
    ./development.nix

    # generated hardware config
    ./hardware-configuration.nix
  ];

  nixpkgs.config.allowUnfree = true;

  nix = {
    nixPath = [
      "nixpkgs=${pkgs.path}"
      "nixos-config=/etc/nixos/configuration.nix"
    ];
    package = pkgs.nixVersions.stable;
    settings = {
      experimental-features = "nix-command flakes";
      auto-optimise-store = true;
    };
  };

  system.autoUpgrade = {
    enable = false;
    allowReboot = false;
  };

  users.users.igor = {
    isNormalUser = true;
    description = "igor";
    extraGroups = [ "networkmanager" "wheel" "docker" "jackaudio" ];
  };

  environment.interactiveShellInit = ''
  alias youtube-dl=yt-dlp
  '';

  environment.systemPackages = with pkgs; [
    google-chrome
    firefox

    ## CLI general utilities
    unzip
    unrar
    ncdu
    fd
    ripgrep
    file
    whois
    tealdeer
    lsof
    jq


    # stow
    # ispell
    telegram-desktop
    vlc
    # anydesk
    # antimicrox

    ## audio/video/image production
    easyeffects
    ffmpeg
    musescore
    gimp
    # guitarix
    audacity
    reaper
    # neural-amp-modeler-lv2
    # kdePackages.kdenlive
    obs-studio

    kdePackages.plasma-browser-integration
    kdePackages.kcalc
    kdePackages.kdeconnect-kde
    blender
    transmission_4-qt6
    bitwarden-desktop
    wl-clipboard-rs

    xournalpp
    logseq

    # mermaid-cli
    # gnuplot
    # sage
    # graphviz

    poppler-utils

    pinentry-qt
    pinentry-emacs
    mpv
    # svp
    yt-dlp
    # activitywatch
    # stremio # disabled because of qtwebengine

    # for exwm
    # scrot
    # slock
    # brightnessctl
    # playerctl
    # alsa-utils

    # postman

    aircrack-ng
    qemu



    nmap


    protonvpn-gui
    # logmein-hamachi
    # haguichi
  ];

  programs.nix-ld = {
    enable = false;
    libraries = with pkgs; [
      stdenv.cc.cc
      stdenv.cc.cc.lib
      pkgs.gcc-unwrapped.lib
    ];
  };

  # systemd.user.services.aw-watcher-window = {
  #   enable = true;
  #   serviceConfig = {
  #     ExecStart = "env aw-watcher-window";
  #   };
  #   wantedBy = [ "graphical-session.target" ];
  # };

  # systemd.user.services.aw-watcher-afk = {
  #   enable = true;
  #   serviceConfig = {
  #     ExecStart = "env aw-watcher-afk";
  #   };
  #   wantedBy = [ "graphical-session.target" ];
  # };

  # systemd.user.services.aw-server = {
  #   enable = true;
  #   serviceConfig = {
  #     ExecStart = "env aw-server";
  #   };
  #   wantedBy = [ "graphical-session.target" ];
  # };

  services.guix.enable = false;

  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    systemService = true;
    user = "igor";
    group = "wheel";
    dataDir = "/home/igor/Sync";
  };

  services.zerotierone = {
    enable = false;
    joinNetworks = [
      "856127940ca9d3fc"
    ];
  };

  # ipfs
  services.kubo = {
    enable = false;
  };

  services.flatpak.enable = false;


  users.defaultUserShell = pkgs.zsh;
  programs.zsh = {
    enable = true;
  };

  programs.gnupg.agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-emacs;
    #enableSSHSupport = true;
  };

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 8000 8080 11470 ];
    allowedUDPPorts = [ ];
  };

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;


  networking.hostName = "acer"; # Define your hostname.

  #fonts.packages = with pkgs; [ nerd-fonts.fira-code ];


  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "pt_BR.UTF-8";
    LC_IDENTIFICATION = "pt_BR.UTF-8";
    LC_MEASUREMENT = "pt_BR.UTF-8";
    LC_MONETARY = "pt_BR.UTF-8";
    LC_NAME = "pt_BR.UTF-8";
    LC_NUMERIC = "pt_BR.UTF-8";
    LC_PAPER = "pt_BR.UTF-8";
    LC_TELEPHONE = "pt_BR.UTF-8";
    LC_TIME = "pt_BR.UTF-8";
  };

  # Enable the KDE Plasma Desktop Environment.
  services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;
  # services.xserver = {
  #   enable = usingEXWM;
  #   # windowManager.xmonad = {
  #   #   enable = true;
  #   #   enableContribAndExtras = true;
  #   #   config = builtins.readFile /home/igor/.config/xmonad/xmonad.hs;
  #   # };
  #   windowManager.session = lib.mkIf usingEXWM (lib.singleton {
  #     name = "EXWM";
  #     start = ''
  #     exec dbus-launch --exit-with-session emacs --fullscreen --eval "(exwm-enable)"
  #     '';
  #   });
  # };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Sound
  services.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  hardware.bluetooth = {
    enable = true;
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?

}
