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
      identifier = { name = "zerepoch-contract"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "jann.mueller@blockchain-company.io";
      author = "Jann Müller";
      homepage = "https://github.com/tbco/zerepoch#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/The-Blockchain-Company/zerepoch#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."zerepoch-chain-index" or (errorHandler.buildDepError "zerepoch-chain-index"))
          (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
          (hsPkgs."zerepoch-ledger" or (errorHandler.buildDepError "zerepoch-ledger"))
          (hsPkgs."zerepoch-ledger-api" or (errorHandler.buildDepError "zerepoch-ledger-api"))
          (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
          (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
          (hsPkgs."bcc-api" or (errorHandler.buildDepError "bcc-api"))
          (hsPkgs."bcc-crypto" or (errorHandler.buildDepError "bcc-crypto"))
          (hsPkgs."bcc-ledger-core" or (errorHandler.buildDepError "bcc-ledger-core"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."fingertree" or (errorHandler.buildDepError "fingertree"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
          (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."quickcheck-dynamic" or (errorHandler.buildDepError "quickcheck-dynamic"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."row-types" or (errorHandler.buildDepError "row-types"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
          (hsPkgs."IntervalMap" or (errorHandler.buildDepError "IntervalMap"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."zerepoch-tx-plugin" or (errorHandler.buildDepError "zerepoch-tx-plugin"))) ++ (pkgs.lib).optionals (!(compiler.isGhcjs && true || system.isGhcjs || system.isWindows)) [
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
          ];
        buildable = true;
        modules = [
          "Data/Row/Extras"
          "Data/Text/Extras"
          "Data/UUID/Extras"
          "Zerepoch/Contract"
          "Zerepoch/Contract/Effects"
          "Zerepoch/Contract/Request"
          "Zerepoch/Contract/Checkpoint"
          "Zerepoch/Contract/Constraints"
          "Zerepoch/Contract/State"
          "Zerepoch/Contract/Schema"
          "Zerepoch/Contract/Trace"
          "Zerepoch/Contract/Trace/RequestHandler"
          "Zerepoch/Contract/Resumable"
          "Zerepoch/Contract/StateMachine"
          "Zerepoch/Contract/StateMachine/OnChain"
          "Zerepoch/Contract/StateMachine/MintingPolarity"
          "Zerepoch/Contract/StateMachine/ThreadToken"
          "Zerepoch/Contract/Tx"
          "Zerepoch/Contract/Types"
          "Zerepoch/Contract/Util"
          "Zerepoch/Contract/Wallet"
          "Zerepoch/Contract/Typed/Tx"
          "Wallet/Emulator"
          "Wallet/Emulator/Types"
          "Wallet/Emulator/Chain"
          "Wallet/Emulator/Error"
          "Wallet/Emulator/Folds"
          "Wallet/Emulator/LogMessages"
          "Wallet/Emulator/NodeClient"
          "Wallet/Emulator/MultiAgent"
          "Wallet/Emulator/Stream"
          "Wallet/Emulator/Wallet"
          "Wallet/Rollup"
          "Wallet/Rollup/Types"
          "Wallet/Rollup/Render"
          "Wallet"
          "Wallet/API"
          "Wallet/Effects"
          "Wallet/Graph"
          "Wallet/Types"
          "Zerepoch/Trace"
          "Zerepoch/Trace/Effects/ContractInstanceId"
          "Zerepoch/Trace/Effects/RunContract"
          "Zerepoch/Trace/Effects/RunContractPlayground"
          "Zerepoch/Trace/Effects/EmulatedWalletAPI"
          "Zerepoch/Trace/Effects/EmulatorControl"
          "Zerepoch/Trace/Effects/Waiting"
          "Zerepoch/Trace/Emulator"
          "Zerepoch/Trace/Emulator/ContractInstance"
          "Zerepoch/Trace/Emulator/Extract"
          "Zerepoch/Trace/Emulator/System"
          "Zerepoch/Trace/Emulator/Types"
          "Zerepoch/Trace/Playground"
          "Zerepoch/Trace/Scheduler"
          "Zerepoch/Trace/Tag"
          ] ++ (pkgs.lib).optionals (!(compiler.isGhcjs && true || system.isGhcjs || system.isWindows)) [
          "Zerepoch/Contract/Test"
          "Zerepoch/Contract/Test/ContractModel"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "zerepoch-contract-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."extensible-effects" or (errorHandler.buildDepError "extensible-effects"))
            (hsPkgs."zerepoch-contract" or (errorHandler.buildDepError "zerepoch-contract"))
            (hsPkgs."zerepoch-ledger" or (errorHandler.buildDepError "zerepoch-ledger"))
            (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."zerepoch-tx-plugin" or (errorHandler.buildDepError "zerepoch-tx-plugin"));
          buildable = true;
          modules = [
            "Spec/Contract"
            "Spec/Emulator"
            "Spec/Rows"
            "Spec/State"
            "Spec/ThreadToken"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../zerepoch-contract; }