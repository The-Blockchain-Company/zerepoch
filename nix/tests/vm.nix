{ pkgs
, zerepoch-playground
, simeon-playground
, simeon-dashboard
, web-ghc
, zerepoch-pab
, simeon-pab
, docs
, vmCompileTests
}:
let
  inherit (pkgs.stdenv) isDarwin;
  testing = import (pkgs.path + "/nixos/lib/testing-python.nix") { system = builtins.currentSystem; };
  makeTest = testing.makeTest;
  tests = pkgs.recurseIntoAttrs {
    zerepoch-playground-server = pkgs.callPackage ./vm-tests/zerepoch-playground.nix { inherit makeTest zerepoch-playground; };
    simeon-playground-server = pkgs.callPackage ./vm-tests/simeon-playground.nix { inherit makeTest simeon-playground; };
    web-ghc = pkgs.callPackage ./vm-tests/web-ghc.nix { inherit makeTest web-ghc; };
    pab = pkgs.callPackage ./vm-tests/pab.nix { inherit makeTest zerepoch-pab simeon-pab simeon-dashboard; };
    all = pkgs.callPackage ./vm-tests/all.nix { inherit makeTest zerepoch-playground simeon-playground simeon-dashboard web-ghc zerepoch-pab simeon-pab docs vmCompileTests; };
  };
in
if isDarwin then { } else tests
