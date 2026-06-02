{ pkgs, lib, ...}:
{
  nixpkgs.overlays = [
    (final: prev: {
      retroarch-bare = prev.retroarch-bare.overrideAttrs (old: {
        version = "master-2026-05-08";
        src = prev.fetchFromGitHub {
          owner = "libretro";
          repo = "RetroArch";
          rev = "d1c4f72747bc0152a6e4d3b95f4cbeddeef466ab";
          hash = "sha256-0/ppwZZHqOdigcqqdGwAInXK45dAfsyF5aoLUUYjxjk=";
        };
      });
    })
    (final: prev: {
      openldap = prev.openldap.overrideAttrs (_: {
        doCheck = !prev.stdenv.hostPlatform.isi686;
      });
    })
  ];

  environment.systemPackages = with pkgs; [
    ## emulation
    (retroarch.withCores (cores: with cores; [
      # genesis-plus-gx # sega genesis / megadrive
      fceumm # nes
      snes9x # snes
      # mupen64plus # n64
      (mupen64plus.overrideAttrs (old: {
        version = "0-unstable-2026-04-02";
        src = pkgs.fetchFromGitHub {
          owner = "libretro";
          repo = "mupen64plus-libretro-nx";
          rev = "58b9daf940fb43f09c3984c6a7c730f4b4c24861";
          hash = "sha256-9d1gbDDK2rOt/a9NNRQVJJmiE+UdohM/yPI5WstNmtA=";
        };
        patches = [];
      }))
      dosbox# dos
      # beetle-gba # gba
      # desmume # nds
      citra # 3ds
      pcsx2 # ps2 (lrps2)
      # fmsx # msx
      # snes9x2010 # snes (fixed version for netplay)
      dolphin # gamecube / wii
      # beetle-psx-hw # ps1 / psx
      # ppsspp # psp
      picodrive # SG-1000, SC-3000, Master System/Mark III, Game Gear, Mega Drive/Genesis, Sega/Mega CD, 32X, Pico
      mrboom # bomberman clone
      # stella2014 # atari 2600
    ]))

    # rpcs3 # ps3
    # pcsx2 # ps2

    vulkan-tools
    mesa-demos

    ## games
    # xonotic
    # clonehero
    # supermariowar

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

  powerManagement.cpuFreqGovernor = "performance";

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
