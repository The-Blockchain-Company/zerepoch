{ pkgs, mkMachine, tfinfo }:
{
  # The network attribute allows to supply
  # some settings to all deployments
  network = {
    description = "zerepoch network";
    inherit pkgs;
  };

  "${tfinfo.simeonDashA.dns}" = mkMachine {
    name = "simeonDashA";
    config = ./machines/simeon-dash.nix;
  };

  "${tfinfo.playgroundsA.dns}" = mkMachine {
    name = "playgroundsB";
    config = ./machines/playground.nix;
  };

  "${tfinfo.webghcA.dns}" = mkMachine {
    name = "webghcA";
    config = ./machines/web-ghc.nix;
  };
}
