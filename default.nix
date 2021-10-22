########################################################################
# default.nix -- The top-level nix build file for Zerepoch.
#
# This file defines various attributes that are used for building and
# developing Zerepoch.
#
########################################################################
{ system ? builtins.currentSystem
, crossSystem ? null
, config ? { allowUnfreePredicate = (import ./nix/lib/unfree.nix).unfreePredicate; }
, sourcesOverride ? { }
, sources ? import ./nix/sources.nix { inherit system; } // sourcesOverride
, haskellNix ? import sources.haskell-nix {
    pkgs = import sources.nixpkgs { inherit system; };
    sourcesOverride = {
      hackage = sources.hackage-nix;
      stackage = sources.stackage-nix;
    };
  }
, packages ? import ./nix { inherit system sources crossSystem config sourcesOverride haskellNix checkMaterialization enableHaskellProfiling; }
  # An explicit git rev to use, passed when we are in Hydra
  # Whether to check that the pinned shas for haskell.nix are correct. We want this to be
  # false, generally, since it does more work, but we set it to true in the CI
, checkMaterialization ? false
  # Whether to build our Haskell packages (and their dependencies) with profiling enabled.
, enableHaskellProfiling ? false
}:
let
  inherit (packages) pkgs zerepoch sources;
  inherit (pkgs) lib haskell-nix;
  inherit (zerepoch) haskell agdaPackages;
  inherit (zerepoch) easyPS sphinxcontrib-haddock;
in
rec {
  inherit pkgs zerepoch;

  inherit (zerepoch) web-ghc;

  inherit (haskell.packages.zerepoch-pab.components.exes)
    zerepoch-pab-examples
    zerepoch-uniswap;

  inherit (haskell.packages.simeon.components.exes) simeon-pab;

  webCommon = pkgs.callPackage ./web-common { inherit (zerepoch.lib) gitignore-nix; };
  webCommonZerepoch = pkgs.callPackage ./web-common-zerepoch { inherit (zerepoch.lib) gitignore-nix; };
  webCommonSimeon = pkgs.callPackage ./web-common-simeon { inherit (zerepoch.lib) gitignore-nix; };
  webCommonPlayground = pkgs.callPackage ./web-common-playground { inherit (zerepoch.lib) gitignore-nix; };

  zerepoch-playground = pkgs.recurseIntoAttrs rec {
    haddock = zerepoch.zerepoch-haddock-combined;

    inherit (pkgs.callPackage ./zerepoch-playground-client {
      inherit (zerepoch.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      inherit haskell webCommon webCommonZerepoch webCommonPlayground;
    }) client server generate-purescript start-backend;
  };

  simeon-playground = pkgs.recurseIntoAttrs rec {
    inherit (pkgs.callPackage ./simeon-playground-client {
      inherit (zerepoch.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      inherit haskell webCommon webCommonSimeon webCommonPlayground;
    }) client server generate-purescript start-backend;
  };

  simeon-dashboard = pkgs.recurseIntoAttrs rec {
    inherit (pkgs.callPackage ./simeon-dashboard-client {
      inherit haskell zerepoch-pab;
      inherit (zerepoch.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      inherit webCommon webCommonSimeon;
    }) client server-setup-invoker simeon-invoker generated-purescript generate-purescript start-backend;
  };

  simeon-dashboard-fake-pab = pkgs.recurseIntoAttrs rec {
    inherit (pkgs.callPackage ./fake-pab {
      inherit simeon-dashboard;
      inherit (zerepoch.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      inherit haskell webCommon webCommonSimeon;
    }) client fake-pab-exe fake-pab-generated-purescript;
  };

  simeon-marketplace = pkgs.recurseIntoAttrs rec {
    inherit (pkgs.callPackage ./simeon-marketplace-client {
      inherit (zerepoch.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      inherit webCommon webCommonSimeon;
    }) client;
  };

  simeon-web = pkgs.callPackage ./simeon-website { inherit (zerepoch.lib) npmlock2nix gitignore-nix; };

  zerepoch-pab = pkgs.recurseIntoAttrs (pkgs.callPackage ./zerepoch-pab-client {
    inherit (zerepoch.lib) buildPursPackage buildNodeModules gitignore-nix filterNpm;
    inherit haskell webCommon webCommonZerepoch;
  });

  zerepoch-use-cases = pkgs.recurseIntoAttrs (pkgs.callPackage ./zerepoch-use-cases {
    inherit haskell;
  });

  tests = import ./nix/tests/default.nix {
    inherit pkgs docs;
    inherit (zerepoch.lib) gitignore-nix;
    inherit (zerepoch) fixStylishHaskell fixPurty fixPngOptimization;
    inherit zerepoch-playground simeon-playground simeon-dashboard web-ghc zerepoch-pab simeon-pab;
    src = ./.;
  };

  docs = import ./nix/docs.nix { inherit pkgs zerepoch; };

  deployment = pkgs.recurseIntoAttrs (pkgs.callPackage ./deployment/morph {
    zerepoch = {
      inherit zerepoch-pab simeon-dashboard simeon-playground zerepoch-playground web-ghc docs simeon-web simeon-pab;
    };
  });

  # This builds a vscode devcontainer that can be used with the zerepoch-starter project (or probably the zerepoch project itself).
  devcontainer = import ./nix/devcontainer/zerepoch-devcontainer.nix { inherit pkgs zerepoch; };

  # Test data needed by simeon-actus provided via niv
  inherit (sources) actus-tests;

  build-and-push-devcontainer-script = import ./nix/devcontainer/deploy/default.nix { inherit pkgs zerepoch; };

  # Packages needed for the bitte deployment
  bitte-packages = import ./bitte { inherit simeon-playground zerepoch-playground web-ghc simeon-pab simeon-dashboard simeon-web docs pkgs; };
}
