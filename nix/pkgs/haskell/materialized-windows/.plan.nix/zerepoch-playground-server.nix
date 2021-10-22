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
      identifier = { name = "zerepoch-playground-server"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "kris.jenkins@tweag.io";
      author = "Kris Jenkins";
      homepage = "https://github.com/bcccoin/zerepoch#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/The-Blockchain-Company/zerepoch#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [
        "usecases/Crowdfunding.hs"
        "usecases/ErrorHandling.hs"
        "usecases/Game.hs"
        "usecases/Vesting.hs"
        "usecases/Starter.hs"
        "test/gists1.json"
        ];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
          (hsPkgs."http-conduit" or (errorHandler.buildDepError "http-conduit"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."jwt" or (errorHandler.buildDepError "jwt"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
          (hsPkgs."playground-common" or (errorHandler.buildDepError "playground-common"))
          (hsPkgs."zerepoch-contract" or (errorHandler.buildDepError "zerepoch-contract"))
          (hsPkgs."zerepoch-ledger" or (errorHandler.buildDepError "zerepoch-ledger"))
          (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
          (hsPkgs."regex-compat" or (errorHandler.buildDepError "regex-compat"))
          (hsPkgs."recursion-schemes" or (errorHandler.buildDepError "recursion-schemes"))
          (hsPkgs."zerepoch-contract" or (errorHandler.buildDepError "zerepoch-contract"))
          (hsPkgs."row-types" or (errorHandler.buildDepError "row-types"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
          (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
          (hsPkgs."servant-purescript" or (errorHandler.buildDepError "servant-purescript"))
          (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."time-units" or (errorHandler.buildDepError "time-units"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
          (hsPkgs."web-ghc" or (errorHandler.buildDepError "web-ghc"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."zerepoch-tx-plugin" or (errorHandler.buildDepError "zerepoch-tx-plugin"));
        buildable = true;
        modules = [
          "Playground/Server"
          "Playground/Interpreter"
          "Playground/Usecases"
          "Crowdfunding"
          "CrowdfundingSimulations"
          "ErrorHandling"
          "ErrorHandlingSimulations"
          "Game"
          "GameSimulations"
          "HelloWorld"
          "HelloWorldSimulations"
          "SimulationUtils"
          "Starter"
          "StarterSimulations"
          "Vesting"
          "VestingSimulations"
          ];
        hsSourceDirs = [ "src" "usecases" ];
        };
      sublibs = {
        "zerepoch-playground-usecases" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."playground-common" or (errorHandler.buildDepError "playground-common"))
            (hsPkgs."zerepoch-contract" or (errorHandler.buildDepError "zerepoch-contract"))
            (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
            (hsPkgs."zerepoch-ledger" or (errorHandler.buildDepError "zerepoch-ledger"))
            (hsPkgs."recursion-schemes" or (errorHandler.buildDepError "recursion-schemes"))
            (hsPkgs."row-types" or (errorHandler.buildDepError "row-types"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."zerepoch-tx-plugin" or (errorHandler.buildDepError "zerepoch-tx-plugin"));
          buildable = true;
          modules = [
            "Crowdfunding"
            "CrowdfundingSimulations"
            "ErrorHandling"
            "ErrorHandlingSimulations"
            "Game"
            "GameSimulations"
            "HelloWorld"
            "HelloWorldSimulations"
            "SimulationUtils"
            "Starter"
            "StarterSimulations"
            "Vesting"
            "VestingSimulations"
            ];
          hsSourceDirs = [ "usecases" ];
          };
        };
      exes = {
        "zerepoch-playground-server" = {
          depends = [
            (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."playground-common" or (errorHandler.buildDepError "playground-common"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
            (hsPkgs."zerepoch-playground-server" or (errorHandler.buildDepError "zerepoch-playground-server"))
            (hsPkgs."zerepoch-playground-server".components.sublibs.zerepoch-playground-usecases or (errorHandler.buildDepError "zerepoch-playground-server:zerepoch-playground-usecases"))
            (hsPkgs."purescript-bridge" or (errorHandler.buildDepError "purescript-bridge"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-foreign" or (errorHandler.buildDepError "servant-foreign"))
            (hsPkgs."servant-purescript" or (errorHandler.buildDepError "servant-purescript"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time-units" or (errorHandler.buildDepError "time-units"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."zerepoch-contract" or (errorHandler.buildDepError "zerepoch-contract"))
            (hsPkgs."zerepoch-ledger" or (errorHandler.buildDepError "zerepoch-ledger"))
            (hsPkgs."recursion-schemes" or (errorHandler.buildDepError "recursion-schemes"))
            (hsPkgs."row-types" or (errorHandler.buildDepError "row-types"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."web-ghc" or (errorHandler.buildDepError "web-ghc"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."zerepoch-tx-plugin" or (errorHandler.buildDepError "zerepoch-tx-plugin"));
          buildable = true;
          modules = [
            "Webserver"
            "Types"
            "PSGenerator"
            "Crowdfunding"
            "CrowdfundingSimulations"
            "ErrorHandling"
            "ErrorHandlingSimulations"
            "Game"
            "GameSimulations"
            "HelloWorld"
            "HelloWorldSimulations"
            "SimulationUtils"
            "Starter"
            "StarterSimulations"
            "Vesting"
            "VestingSimulations"
            ];
          hsSourceDirs = [ "app" "usecases" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) "";
          };
        };
      tests = {
        "zerepoch-playground-server-test" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."insert-ordered-containers" or (errorHandler.buildDepError "insert-ordered-containers"))
            (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
            (hsPkgs."playground-common" or (errorHandler.buildDepError "playground-common"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."zerepoch-playground-server" or (errorHandler.buildDepError "zerepoch-playground-server"))
            (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
            (hsPkgs."zerepoch-ledger" or (errorHandler.buildDepError "zerepoch-ledger"))
            (hsPkgs."zerepoch-playground-server".components.sublibs.zerepoch-playground-usecases or (errorHandler.buildDepError "zerepoch-playground-server:zerepoch-playground-usecases"))
            (hsPkgs."zerepoch-contract" or (errorHandler.buildDepError "zerepoch-contract"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time-units" or (errorHandler.buildDepError "time-units"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."zerepoch-ledger" or (errorHandler.buildDepError "zerepoch-ledger"))
            (hsPkgs."web-ghc" or (errorHandler.buildDepError "web-ghc"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."zerepoch-tx-plugin" or (errorHandler.buildDepError "zerepoch-tx-plugin"));
          buildable = true;
          modules = [
            "GistSpec"
            "Paths_zerepoch_playground_server"
            "Playground/InterpreterSpec"
            "Playground/UsecasesSpec"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../zerepoch-playground-server; }