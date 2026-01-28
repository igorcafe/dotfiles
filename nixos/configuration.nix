# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ lib, pkgs, ... }:
{
  imports = [
    ./hardware-configuration_amdpc.nix
  ];

  nix = {
    settings = {
      experimental-features = "nix-command flakes";
      auto-optimise-store = true;
    };
  };

  zramSwap.enable = false;

  # nixpkgs.pkgs = pkgs;
  # nixpkgs.overlays = [
  #   (prev: final: {
  #     rtw88-firmware = final.linux-firmware;
  #   })
  # ];
  nixpkgs.config.allowUnfree = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  # sound.enable = true;
  services.pulseaudio.enable = false;
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


  environment.systemPackages = with pkgs; [
    # (retroarch.override {
    #   cores = with libretro; [
    #     # TODO:
    #     # dinothaw
    #     # game gear
    #     # sega 32x
    #     # sega cd
    #     # atari 7800
    #     # atari lynx
    #     # pc engine
    #     # nintendo ds
    #     # famicom disk system
    #     # gb, gba, vb
    #     # fbneo
    #     # mame
    #     # phantomsystem

    #     citra
    #     fmsx
    #     dosbox
    #     fceumm
    #     snes9x
    #     snes9x2010
    #     mupen64plus
    #     dolphin
    #     beetle-psx-hw
    #     pcsx2
    #     ppsspp
    #     genesis-plus-gx
    #     picodrive
    #     mrboom
    #     stella2014
    #   ];
    # })

    ncdu
    neovim
    gnumake
    vulkan-tools
    go
    fd
    libnotify
    python3
    git
    ripgrep
    mangohud
    jq
    #xonotic
    direnv
    # unstable.kdePackages.kdenlive
    tealdeer
    unrar
    unzip
    htop
    fzf
    p7zip
    google-chrome
    lsd
    obs-studio
    ffmpeg
    gimp
    vlc
    telegram-desktop
    file

    bottles
    lutris
    umu-launcher
    protonup-qt
    ryubing
  ];

  services.ollama = {
    enable = false;
    # acceleration = "rocm";
  };

  services.emacs = {
    enable = true;
    package = (pkgs.emacs-gtk.pkgs.withPackages (epkgs: with epkgs; [
        melpaPackages.telega
        vterm
        ement
    ]));
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
    ];
  };

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = with pkgs; [
      rocmPackages.clr.icd
      # amdvlk
    ];
    extraPackages32 = with pkgs; [
      # driversi686Linux.amdvlk
    ];
  };

  hardware.bluetooth = {
    enable = true;
    settings = {
      General = {
        ControllerMode = "bredr";
      };
    };
  };


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
  services.displayManager.sddm.enable = true;

  services.xserver = {
    # Enable the X11 windowing system.
    enable = true;
    videoDrivers = ["amdgpu"];

    # Configure keymap in X11
    xkb.layout = "us";
    xkb.variant = "";
  };



  # hardware.graphics = {
  #   enable = true;
  #   enable32Bit = true;
  # };

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # kernel
  boot.kernelPackages = pkgs.linuxPackages_xanmod_latest;
  boot.kernelParams = [
    # Enable "Smart Access Memory" / Resizable BAR support if your CPU/Motherboard supports it
    # This can give a 5-10% performance boost in some games.
    "amd_iommu=on"
  ];

  services.scx = {
    enable = true;
    # bpfland is the current speed leader for raw single-app throughput
    scheduler = "scx_bpfland";
    # -m 1: Gaming mode (prioritizes high-utilization threads)
    # -p: Performance mode (prevents cores from downclocking)
    # -x: Experimental "fast path" logic
    extraArgs = [ "-m" "1" "-p" "-x" ]; 
  };

  environment.variables = {
    # Forces the use of the RADV driver
    AMD_VULKAN_ICD = "RADV";
  };

  # Ensure the firmware for your specific card is available
  hardware.enableRedistributableFirmware = true;

  networking.hostName = "amdpc"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;


  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 8000 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?

}
