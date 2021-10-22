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
      identifier = { name = "simeon-actus"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "dmytro.kondratiuk@bcccoin.io";
      author = "Dmytro Kondratiuk";
      homepage = "";
      url = "";
      synopsis = "Simeon ACTUS: standardised financial contracts on Bcc Computation Layer";
      description = "implementation of ACTUS contracts on Simeon";
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
          (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
          (hsPkgs."zerepoch-contract" or (errorHandler.buildDepError "zerepoch-contract"))
          (hsPkgs."zerepoch-ledger" or (errorHandler.buildDepError "zerepoch-ledger"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."simeon" or (errorHandler.buildDepError "simeon"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."sort" or (errorHandler.buildDepError "sort"))
          (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
          ];
        buildable = true;
        modules = [
          "Language/Simeon/ACTUS/Ops"
          "Language/Simeon/ACTUS/SimeonCompat"
          "Language/Simeon/ACTUS/Generator"
          "Language/Simeon/ACTUS/Analysis"
          "Language/Simeon/ACTUS/Definitions/BusinessEvents"
          "Language/Simeon/ACTUS/Definitions/ContractTerms"
          "Language/Simeon/ACTUS/Definitions/ContractState"
          "Language/Simeon/ACTUS/Definitions/Schedule"
          "Language/Simeon/ACTUS/Model/POF/PayoffModel"
          "Language/Simeon/ACTUS/Model/POF/Payoff"
          "Language/Simeon/ACTUS/Model/POF/PayoffFs"
          "Language/Simeon/ACTUS/Model/STF/StateTransitionModel"
          "Language/Simeon/ACTUS/Model/STF/StateTransition"
          "Language/Simeon/ACTUS/Model/STF/StateTransitionFs"
          "Language/Simeon/ACTUS/Model/SCHED/ContractScheduleModel"
          "Language/Simeon/ACTUS/Model/SCHED/ContractSchedule"
          "Language/Simeon/ACTUS/Model/INIT/StateInitializationModel"
          "Language/Simeon/ACTUS/Model/APPLICABILITY/Applicability"
          "Language/Simeon/ACTUS/Model/APPLICABILITY/ApplicabilityModel"
          "Language/Simeon/ACTUS/Model/Utility/ANN/Annuity"
          "Language/Simeon/ACTUS/Model/Utility/DateShift"
          "Language/Simeon/ACTUS/Model/Utility/ScheduleGenerator"
          "Language/Simeon/ACTUS/Model/Utility/YearFraction"
          "Language/Simeon/ACTUS/Model/Utility/ContractRoleSign"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "simeon-shiny" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deriving-aeson" or (errorHandler.buildDepError "deriving-aeson"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
            (hsPkgs."zerepoch-contract" or (errorHandler.buildDepError "zerepoch-contract"))
            (hsPkgs."zerepoch-ledger" or (errorHandler.buildDepError "zerepoch-ledger"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
            (hsPkgs."wl-pprint" or (errorHandler.buildDepError "wl-pprint"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."simeon" or (errorHandler.buildDepError "simeon"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."loch-th" or (errorHandler.buildDepError "loch-th"))
            (hsPkgs."sort" or (errorHandler.buildDepError "sort"))
            (hsPkgs."inline-r" or (errorHandler.buildDepError "inline-r"))
            (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
            ];
          buildable = true;
          modules = [
            "Language/Simeon/ACTUS/SimeonCompat"
            "Language/Simeon/ACTUS/Generator"
            "Language/Simeon/ACTUS/Analysis"
            "Language/Simeon/ACTUS/Definitions/BusinessEvents"
            "Language/Simeon/ACTUS/Definitions/ContractTerms"
            "Language/Simeon/ACTUS/Definitions/ContractState"
            "Language/Simeon/ACTUS/Definitions/Schedule"
            "Language/Simeon/ACTUS/Model/APPLICABILITY/Applicability"
            "Language/Simeon/ACTUS/Model/APPLICABILITY/ApplicabilityModel"
            "Language/Simeon/ACTUS/Model/POF/PayoffModel"
            "Language/Simeon/ACTUS/Model/POF/Payoff"
            "Language/Simeon/ACTUS/Model/POF/PayoffFs"
            "Language/Simeon/ACTUS/Model/STF/StateTransitionModel"
            "Language/Simeon/ACTUS/Model/STF/StateTransition"
            "Language/Simeon/ACTUS/Model/STF/StateTransitionFs"
            "Language/Simeon/ACTUS/Model/SCHED/ContractScheduleModel"
            "Language/Simeon/ACTUS/Model/SCHED/ContractSchedule"
            "Language/Simeon/ACTUS/Model/INIT/StateInitializationModel"
            "Language/Simeon/ACTUS/Model/Utility/ANN/Annuity"
            "Language/Simeon/ACTUS/Model/Utility/DateShift"
            "Language/Simeon/ACTUS/Model/Utility/ScheduleGenerator"
            "Language/Simeon/ACTUS/Model/Utility/YearFraction"
            "Language/Simeon/ACTUS/Model/Utility/ContractRoleSign"
            "Language/Simeon/ACTUS/Ops"
            ];
          hsSourceDirs = [ "app" "src" ];
          mainPath = [ "Main.hs" ];
          };
        "simeon-actus-test-kit" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
            (hsPkgs."zerepoch-contract" or (errorHandler.buildDepError "zerepoch-contract"))
            (hsPkgs."zerepoch-ledger" or (errorHandler.buildDepError "zerepoch-ledger"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."simeon" or (errorHandler.buildDepError "simeon"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."sort" or (errorHandler.buildDepError "sort"))
            (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          modules = [
            "Language/Simeon/ACTUS/SimeonCompat"
            "Language/Simeon/ACTUS/Generator"
            "Language/Simeon/ACTUS/QCGenerator"
            "Language/Simeon/ACTUS/Analysis"
            "Language/Simeon/ACTUS/Definitions/BusinessEvents"
            "Language/Simeon/ACTUS/Definitions/ContractTerms"
            "Language/Simeon/ACTUS/Definitions/ContractState"
            "Language/Simeon/ACTUS/Definitions/Schedule"
            "Language/Simeon/ACTUS/Model/APPLICABILITY/Applicability"
            "Language/Simeon/ACTUS/Model/APPLICABILITY/ApplicabilityModel"
            "Language/Simeon/ACTUS/Model/POF/PayoffModel"
            "Language/Simeon/ACTUS/Model/POF/Payoff"
            "Language/Simeon/ACTUS/Model/POF/PayoffFs"
            "Language/Simeon/ACTUS/Model/STF/StateTransitionModel"
            "Language/Simeon/ACTUS/Model/STF/StateTransition"
            "Language/Simeon/ACTUS/Model/STF/StateTransitionFs"
            "Language/Simeon/ACTUS/Model/SCHED/ContractScheduleModel"
            "Language/Simeon/ACTUS/Model/SCHED/ContractSchedule"
            "Language/Simeon/ACTUS/Model/INIT/StateInitializationModel"
            "Language/Simeon/ACTUS/Model/Utility/ANN/Annuity"
            "Language/Simeon/ACTUS/Model/Utility/DateShift"
            "Language/Simeon/ACTUS/Model/Utility/ScheduleGenerator"
            "Language/Simeon/ACTUS/Model/Utility/YearFraction"
            "Language/Simeon/ACTUS/Model/Utility/ContractRoleSign"
            "Language/Simeon/ACTUS/Ops"
            ];
          hsSourceDirs = [ "testkit" "src" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "simeon-actus-test" = {
          depends = [
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."zerepoch-ledger" or (errorHandler.buildDepError "zerepoch-ledger"))
            (hsPkgs."zerepoch-contract" or (errorHandler.buildDepError "zerepoch-contract"))
            (hsPkgs."simeon" or (errorHandler.buildDepError "simeon"))
            (hsPkgs."simeon-actus" or (errorHandler.buildDepError "simeon-actus"))
            (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."simeon" or (errorHandler.buildDepError "simeon"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
            ];
          buildable = true;
          modules = [ "Spec/Simeon/Util" "Spec/Simeon/Actus" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../simeon-actus; }