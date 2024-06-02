{pkgs, inputs, ...}: {
  nixpkgs.config.allowUnfree = true;

  nixpkgs.overlays = [
    inputs.telega-overlay.overlay
  ];

  nix = {
    package = pkgs.nixFlakes;
    settings = {
      experimental-features = "nix-command flakes";
      auto-optimise-store = true;
    };
  };

  environment.systemPackages = with pkgs; [
    (retroarch.override {
      cores = with libretro; [
        # TODO:
        # dinothaw
        # game gear
        # sega 32x
        # sega cd
        # atari 7800
        # atari lynx
        # pc engine
        # nintendo ds
        # famicom disk system
        # gb, gba, vb
        # fbneo
        # mame
        # phantomsystem

        fmsx
        dosbox
        fceumm
        snes9x
        snes9x2010
        mupen64plus
        dolphin
        beetle-psx-hw
        pcsx2
        ppsspp
        genesis-plus-gx
        picodrive
        mrboom
        stella2014
      ];
    })
    ncdu
    neovim
    gnumake
    vulkan-tools

    # emacs
    (emacs-gtk.pkgs.withPackages (epkgs: with epkgs; [
        melpaPackages.telega
        vterm
        ement
    ]))
    ]))

    # go
    go
    gopls
  ];

  services.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
  };

  users.defaultUserShell = pkgs.zsh;
  programs.zsh.enable = true;

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
  };

  programs.gamemode.enable = true;

  virtualisation.docker = {
    enable = true;
  };

  users.users.user = {
    isNormalUser = true;
    description = "user";
    extraGroups = [ "networkmanager" "wheel" "docker" ];
    packages = with pkgs; [
      firefox
      kate
    ];
  };

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };

  hardware.bluetooth = {
    enable = true;
    settings = {
      General = {
        ControllerMode = "bredr";
      };
    };
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

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


  services.desktopManager.plasma6.enable = true;
  services.xserver = {
    # Enable the X11 windowing system.
    enable = true;

    # Enable the KDE Plasma Desktop Environment.
    displayManager.sddm.enable = true;

    # Configure keymap in X11
    xkb.layout = "us";
    xkb.variant = "";
  };
}
