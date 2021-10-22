{ pkgs, zerepoch, tfinfo, extraImports ? [ ] }:
# mkMachine :: { config : Path, name : String } -> NixOS machine
# Takes a machine specific configuration and a hostname to set and
# applies generic settings:
# - aws machine settings from ./profiles/std.nix
# - configures root ssh keys for
# - adds zerepoch specific packages through an overlay
{ config, name }: {
  imports = extraImports ++ [

    (pkgs.path + "/nixos/modules/virtualisation/amazon-image.nix")

    config

    ({ config, ... }: {
      config._module.args.tfinfo = tfinfo;
    })

    ({ lib, config, ... }:
      {
        networking.hostName = name;
        users.extraUsers.root.openssh.authorizedKeys.keys = tfinfo.rootSshKeys;
        nixpkgs = {
          inherit pkgs;
          overlays = [
            (self: super: {
              simeon-pab = zerepoch.simeon-pab;
              zerepoch-pab = zerepoch.zerepoch-pab;
              simeon-dashboard = zerepoch.simeon-dashboard;
              simeon-playground = zerepoch.simeon-playground;
              simeon-web = zerepoch.simeon-web;
              zerepoch-playground = zerepoch.zerepoch-playground;
              web-ghc = zerepoch.web-ghc;
              zerepoch-docs = zerepoch.docs;
            })
          ];
        };
      })
  ];
}
