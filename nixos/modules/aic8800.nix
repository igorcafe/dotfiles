{ config, pkgs, ... }:

let
  kernel = config.boot.kernelPackages.kernel;
  src = pkgs.fetchFromGitHub {
    owner = "kilam994";
    repo = "aic8800d80-linux-driver";
    rev = "65f74ccbe982d9242afd1b3126a28fc3a77abb4f";
    hash = "sha256-/IMPe4aJspmA9fVWmaQk0px/71T/XYpHxcbx/YsC1n0=";
  };

  aic8800 = config.boot.kernelPackages.stdenv.mkDerivation {
    pname = "aic8800";
    version = "unstable-2026-06-04";
    inherit src;

    nativeBuildInputs = kernel.moduleBuildDependencies;

    postPatch = ''
      substituteInPlace drivers/aic8800/aic_load_fw/aicbluetooth.c \
        --replace-fail '"/lib/firmware"' '"/run/current-system/firmware"'
    '';

    buildPhase = ''
      runHook preBuild
      make -C drivers/aic8800 KVER=${kernel.modDirVersion} KDIR=${kernel.dev}/lib/modules/${kernel.modDirVersion}/build
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      install -Dm444 drivers/aic8800/aic_load_fw/aic_load_fw.ko $out/lib/modules/${kernel.modDirVersion}/extra/aic8800/aic_load_fw.ko
      install -Dm444 drivers/aic8800/aic8800_fdrv/aic8800_fdrv.ko $out/lib/modules/${kernel.modDirVersion}/extra/aic8800/aic8800_fdrv.ko
      runHook postInstall
    '';
  };

  firmware = pkgs.stdenvNoCC.mkDerivation {
    pname = "aic8800-firmware";
    version = "unstable-2026-06-04";
    inherit src;

    compressFirmware = false;

    installPhase = ''
      runHook preInstall
      mkdir -p $out/lib/firmware
      cp -r fw/aic8800* $out/lib/firmware/
      runHook postInstall
    '';
  };
in
{
  boot.extraModulePackages = [ aic8800 ];
  boot.kernelModules = [ "aic_load_fw" "aic8800_fdrv" ];

  hardware.firmware = [ firmware ];

  environment.systemPackages = with pkgs; [
    usb-modeswitch
    usbutils
  ];

  environment.etc."usb_modeswitch.d/1111:1111".text = ''
    DefaultVendor=0x1111
    DefaultProduct=0x1111
    TargetVendor=0xa69c
    TargetProduct=0x8d80
    MessageContent="555342438765432100000000000010fd0000000000000000000000000000f2"
  '';

  services.udev.extraRules = ''
    KERNEL=="sd*", ATTRS{idVendor}=="a69c", ATTRS{idProduct}=="5721", SYMLINK+="aicudisk", RUN+="${pkgs.util-linux}/bin/eject /dev/%k"
    KERNEL=="sd*", ATTRS{idVendor}=="a69c", ATTRS{idProduct}=="5723", SYMLINK+="tendaudisk", RUN+="${pkgs.util-linux}/bin/eject /dev/%k"
    KERNEL=="sd*", ATTRS{idVendor}=="a69c", ATTRS{idProduct}=="5724", SYMLINK+="ugreenax900", RUN+="${pkgs.util-linux}/bin/eject /dev/%k"
    KERNEL=="sd*", ATTRS{idVendor}=="a69c", ATTRS{idProduct}=="5725", SYMLINK+="tendaudiskv2", RUN+="${pkgs.util-linux}/bin/eject /dev/%k"
    KERNEL=="sd*", ATTRS{idVendor}=="a69c", ATTRS{idProduct}=="5726", SYMLINK+="tendaudiskv3", RUN+="${pkgs.util-linux}/bin/eject /dev/%k"
    KERNEL=="sd*", ATTRS{idVendor}=="a69c", ATTRS{idProduct}=="5727", SYMLINK+="tendaudiskv4", RUN+="${pkgs.util-linux}/bin/eject /dev/%k"
    KERNEL=="sd*", ATTRS{idVendor}=="a69c", ATTRS{idProduct}=="572a", SYMLINK+="tendaudiskv5", RUN+="${pkgs.util-linux}/bin/eject /dev/%k"
    KERNEL=="sd*", ATTRS{idVendor}=="a69c", ATTRS{idProduct}=="572c", SYMLINK+="cudydiskv2", RUN+="${pkgs.util-linux}/bin/eject /dev/%k"
  '';
}
