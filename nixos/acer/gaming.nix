{ pkgs, ...}:
{
  environment.systemPackages = with pkgs; [
    ## emulation
    (retroarch.withCores (cores: with cores; [
      genesis-plus-gx
      fceumm
      snes9x
      (mupen64plus.overrideAttrs (old: {
        version = "222acbd3f98391458a047874d0372fe78e14fe94";
        src = pkgs.fetchFromGitHub {
          owner = "libretro";
          repo = "mupen64plus-libretro-nx";
          rev = "222acbd3f98391458a047874d0372fe78e14fe94";
          hash = "sha256-esssh/0nxNUDW/eMDQbWEdcSPuqLjnKLkK4mKN17HjQ=";
        };
      }))
      citra
      dosbox
      beetle-gba
      # cores.desmume
      # cores.pcsx2
      # cores.beetle-psx
    ]))

    vulkan-tools
    mesa-demos

    ## games
    xonotic
    clonehero
    supermariowar

    ## game launchers and compatibility layers
    lutris
    # heroic = heroic-2_19.heroic;

    mangohud
  ];

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    gamescopeSession.enable = true;
  };

  environment.sessionVariables = {
    STEAM_EXTRA_COMPAT_TOOLS_PATHS = "$HOME/.steam/root/compatibilitytools.d";
    LIBVA_DRIVER_NAME = "iHD";
  };

  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      # Required for modern Intel GPUs (Xe iGPU and ARC)
      intel-media-driver     # VA-API (iHD) userspace
      vpl-gpu-rt             # oneVPL (QSV) runtime

      # Optional (compute / tooling):
      intel-compute-runtime  # OpenCL (NEO) + Level Zero for Arc/Xe
      # NOTE: 'intel-ocl' also exists as a legacy package; not recommended for Arc/Xe.
      # libvdpau-va-gl       # Only if you must run VDPAU-only apps
    ];
  };

  services.ananicy.enable = true;
  services.throttled.enable = true;
  programs.gamemode.enable = true;
  programs.gamescope.enable = true;

  zramSwap = {
    enable = true;
    memoryPercent = 200;
    swapDevices = 1;
    priority = 32000;
  };

  # kernel
  boot.kernelPackages = pkgs.linuxPackages_xanmod_latest;
  boot.kernelParams = ["i915.force_probe=46a8"];

  security.rtkit.enable = true;
  services.power-profiles-daemon.enable = false;

  services.tlp = {
    enable = false;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

      CPU_ENERGY_PERF_POLICY_ON_AC = "power";
      CPU_ENERGY_PERF_POLICY_ON_BAT = "powersave";

      CPU_MIN_PERF_ON_AC = 0;
      CPU_MAX_PERF_ON_AC = 100;
      CPU_MIN_PERF_ON_BAT = 0;
      CPU_MAX_PERF_ON_BAT = 20;

      START_CHARGE_THRESH_BAT0 = 30;
      STOP_CHARGE_THRESH_BAT0 = 80;
      USB_AUTOSUSPEND = 1;
    };
  };
}
