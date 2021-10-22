{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { defer-plugin-errors = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "simeon"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "alexander.nemish@bcccoin.io";
      author = "Alexander Nemish";
      homepage = "";
      url = "";
      synopsis = "Simeon: financial contracts on Bcc Computation Layer";
      description = "A reference implementation of Simeon, domain-specific language targeted at\nthe execution of financial contracts in the style of Peyton Jones et al\non Bcc Computation Layer.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [ "README.md" ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."deriving-aeson" or (errorHandler.buildDepError "deriving-aeson"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."zerepoch-chain-index" or (errorHandler.buildDepError "zerepoch-chain-index"))
          (hsPkgs."zerepoch-contract" or (errorHandler.buildDepError "zerepoch-contract"))
          (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
          (hsPkgs."zerepoch-ledger" or (errorHandler.buildDepError "zerepoch-ledger"))
          (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
          (hsPkgs."zerepoch-use-cases" or (errorHandler.buildDepError "zerepoch-use-cases"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."wl-pprint" or (errorHandler.buildDepError "wl-pprint"))
          (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."zerepoch-tx-plugin" or (errorHandler.buildDepError "zerepoch-tx-plugin"));
        buildable = true;
        modules = [
          "Language/Simeon"
          "Language/Simeon/Extended"
          "Language/Simeon/Semantics"
          "Language/Simeon/Client"
          "Language/Simeon/Util"
          "Language/Simeon/Pretty"
          "Language/Simeon/Analysis/FSSemantics"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "simeon-pab" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
            (hsPkgs."zerepoch-contract" or (errorHandler.buildDepError "zerepoch-contract"))
            (hsPkgs."zerepoch-ledger" or (errorHandler.buildDepError "zerepoch-ledger"))
            (hsPkgs."zerepoch-pab" or (errorHandler.buildDepError "zerepoch-pab"))
            (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
            (hsPkgs."purescript-bridge" or (errorHandler.buildDepError "purescript-bridge"))
            (hsPkgs."simeon" or (errorHandler.buildDepError "simeon"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."zerepoch-tx-plugin" or (errorHandler.buildDepError "zerepoch-tx-plugin"));
          buildable = true;
          modules = [ "SimeonContract" ];
          hsSourceDirs = [ "pab" ];
          mainPath = ([
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.defer-plugin-errors) "") ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) "";
          };
        };
      tests = {
        "simeon-test-long-running" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."hint" or (errorHandler.buildDepError "hint"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."zerepoch-chain-index" or (errorHandler.buildDepError "zerepoch-chain-index"))
            (hsPkgs."zerepoch-contract" or (errorHandler.buildDepError "zerepoch-contract"))
            (hsPkgs."zerepoch-ledger" or (errorHandler.buildDepError "zerepoch-ledger"))
            (hsPkgs."simeon" or (errorHandler.buildDepError "simeon"))
            (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
            (hsPkgs."zerepoch-pab" or (errorHandler.buildDepError "zerepoch-pab"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."purescript-bridge" or (errorHandler.buildDepError "purescript-bridge"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ];
          buildable = true;
          modules = [ "Spec/PAB/Workflow" "SimeonContract" ];
          hsSourceDirs = [ "test" "pab" ];
          mainPath = [ "SpecLongRunning.hs" ];
          };
        "simeon-test" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."hint" or (errorHandler.buildDepError "hint"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."zerepoch-chain-index" or (errorHandler.buildDepError "zerepoch-chain-index"))
            (hsPkgs."zerepoch-contract" or (errorHandler.buildDepError "zerepoch-contract"))
            (hsPkgs."zerepoch-ledger" or (errorHandler.buildDepError "zerepoch-ledger"))
            (hsPkgs."simeon" or (errorHandler.buildDepError "simeon"))
            (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
            (hsPkgs."zerepoch-pab" or (errorHandler.buildDepError "zerepoch-pab"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."purescript-bridge" or (errorHandler.buildDepError "purescript-bridge"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ];
          buildable = true;
          modules = [
            "Spec/Simeon/Common"
            "Spec/Simeon/Simeon"
            "Spec/Simeon/AutoExecute"
            "SimeonContract"
            ];
          hsSourceDirs = [ "test" "pab" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../simeon; }