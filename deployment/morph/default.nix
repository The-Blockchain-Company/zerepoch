{ pkgs, zerepoch }:
let
  # Dummy definition of what is usually read from
  # the terraform local resource `machines.json`.
  # The attributes in below are read in `machines.nix`
  tfinfo = {
    rootSshKeys = [ ];
    rev = "dev";
    simeonDashA.dns = "simeon-dash-a";
    simeonDashB.dns = "simeon-dash-b";
    playgroundsA.dns = "playgrounds-a";
    playgroundsB.dns = "playgrounds-b";
    webghcA.dns = "webghc-a";
    webghcB.dns = "webghc-b";
    environment = "alpha";
    zerepochTld = "zerepoch.tbcodev.io";
    simeonTld = "simeon.tbcodev.io";
  };

  # Fake `deployment` option definition so `pkgs.nixos` does not
  # fail building the machines when it encounters the `deployment`.
  fakeDeploymentOption = { lib, config, ... }: {
    options.deployment = lib.mkOption {
      type = lib.types.attrs;
      description = "fake";
    };
  };

  # Get a `buildMachine` function that wraps a `mkMachine` call with the fake deployment option
  # in a `pkgs.nixos` call to build the machine outside of morph.
  mkMachine = pkgs.callPackage ./mk-machine.nix { inherit zerepoch tfinfo; extraImports = [ fakeDeploymentOption ]; };
  buildMachine = { config, name }: (pkgs.nixos (mkMachine { inherit config name; })).toplevel;
  linuxOnly = x: if pkgs.stdenv.isLinux then x else { };
in
linuxOnly (import ./machines.nix {
  inherit pkgs tfinfo;
  mkMachine = buildMachine;
})
