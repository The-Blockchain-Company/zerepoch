let
  zerepoch = import ../../. { };
  pkgs = zerepoch.pkgs;
  tfinfo = builtins.fromJSON (builtins.readFile ./machines.json);
  mkMachine = pkgs.callPackage ./mk-machine.nix {
    inherit zerepoch tfinfo;
  };
in
import ./machines.nix {
  inherit pkgs mkMachine tfinfo;
}
