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
      specVersion = "2.0";
      identifier = { name = "zerepoch-use-cases"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "jann.mueller@blockchain-company.io";
      author = "Manuel M T Chakravarty, Jann Müller";
      homepage = "";
      url = "";
      synopsis = "Collection of smart contracts to develop the zerepoch/wallet interface";
      description = "Collection of smart contracts to develop the zerepoch/wallet interface.";
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
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
          (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
          (hsPkgs."zerepoch-contract" or (errorHandler.buildDepError "zerepoch-contract"))
          (hsPkgs."playground-common" or (errorHandler.buildDepError "playground-common"))
          (hsPkgs."zerepoch-ledger" or (errorHandler.buildDepError "zerepoch-ledger"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."zerepoch-tx-plugin" or (errorHandler.buildDepError "zerepoch-tx-plugin"));
        buildable = true;
        modules = [
          "Zerepoch/Contracts"
          "Zerepoch/Contracts/Auction"
          "Zerepoch/Contracts/TokenAccount"
          "Zerepoch/Contracts/Crowdfunding"
          "Zerepoch/Contracts/Currency"
          "Zerepoch/Contracts/ErrorHandling"
          "Zerepoch/Contracts/Escrow"
          "Zerepoch/Contracts/SimpleEscrow"
          "Zerepoch/Contracts/Future"
          "Zerepoch/Contracts/GameStateMachine"
          "Zerepoch/Contracts/Governance"
          "Zerepoch/Contracts/MultiSig"
          "Zerepoch/Contracts/MultiSigStateMachine"
          "Zerepoch/Contracts/PingPong"
          "Zerepoch/Contracts/Prism"
          "Zerepoch/Contracts/Prism/Credential"
          "Zerepoch/Contracts/Prism/STO"
          "Zerepoch/Contracts/Prism/Mirror"
          "Zerepoch/Contracts/Prism/StateMachine"
          "Zerepoch/Contracts/Prism/Unlock"
          "Zerepoch/Contracts/PubKey"
          "Zerepoch/Contracts/Stablecoin"
          "Zerepoch/Contracts/Swap"
          "Zerepoch/Contracts/Uniswap"
          "Zerepoch/Contracts/Uniswap/OnChain"
          "Zerepoch/Contracts/Uniswap/OffChain"
          "Zerepoch/Contracts/Uniswap/Pool"
          "Zerepoch/Contracts/Uniswap/Trace"
          "Zerepoch/Contracts/Uniswap/Types"
          "Zerepoch/Contracts/Vesting"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "zerepoch-use-cases-scripts" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."row-types" or (errorHandler.buildDepError "row-types"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
            (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."zerepoch-ledger-api" or (errorHandler.buildDepError "zerepoch-ledger-api"))
            (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
            (hsPkgs."zerepoch-contract" or (errorHandler.buildDepError "zerepoch-contract"))
            (hsPkgs."zerepoch-chain-index" or (errorHandler.buildDepError "zerepoch-chain-index"))
            (hsPkgs."zerepoch-ledger" or (errorHandler.buildDepError "zerepoch-ledger"))
            (hsPkgs."zerepoch-use-cases" or (errorHandler.buildDepError "zerepoch-use-cases"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."bcc-api" or (errorHandler.buildDepError "bcc-api"))
            (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."zerepoch-tx-plugin" or (errorHandler.buildDepError "zerepoch-tx-plugin"));
          buildable = true;
          modules = [
            "Spec/Auction"
            "Spec/Crowdfunding"
            "Spec/Currency"
            "Spec/ErrorHandling"
            "Spec/Escrow"
            "Spec/SimpleEscrow"
            "Spec/Future"
            "Spec/GameStateMachine"
            "Spec/Governance"
            "Spec/MultiSig"
            "Spec/MultiSigStateMachine"
            "Spec/PingPong"
            "Spec/PubKey"
            "Spec/Prism"
            "Spec/Rollup"
            "Spec/Stablecoin"
            "Spec/TokenAccount"
            "Spec/Vesting"
            ];
          hsSourceDirs = [ "scripts" "test" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) "";
          };
        };
      tests = {
        "zerepoch-use-cases-test" = {
          depends = [
            (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
            (hsPkgs."zerepoch-contract" or (errorHandler.buildDepError "zerepoch-contract"))
            (hsPkgs."zerepoch-ledger" or (errorHandler.buildDepError "zerepoch-ledger"))
            (hsPkgs."zerepoch-use-cases" or (errorHandler.buildDepError "zerepoch-use-cases"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
            (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."zerepoch-tx-plugin" or (errorHandler.buildDepError "zerepoch-tx-plugin"));
          buildable = true;
          modules = [
            "Spec/Auction"
            "Spec/Crowdfunding"
            "Spec/Currency"
            "Spec/ErrorHandling"
            "Spec/Escrow"
            "Spec/SimpleEscrow"
            "Spec/Future"
            "Spec/GameStateMachine"
            "Spec/Governance"
            "Spec/MultiSig"
            "Spec/MultiSigStateMachine"
            "Spec/PingPong"
            "Spec/PubKey"
            "Spec/Prism"
            "Spec/Rollup"
            "Spec/Stablecoin"
            "Spec/Uniswap"
            "Spec/TokenAccount"
            "Spec/Vesting"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../zerepoch-use-cases; }