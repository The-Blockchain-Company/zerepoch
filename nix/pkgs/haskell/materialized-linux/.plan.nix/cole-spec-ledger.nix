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
    flags = { development = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "cole-spec-ledger"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "dev@blockchain-company.io";
      author = "The Blockchain Co. Formal Methods Team";
      homepage = "https://github.com/The-Blockchain-Company/bcc-legder-specs";
      url = "";
      synopsis = "Executable specification of Bcc ledger";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "CHANGELOG.md" "src/shepard_genomes/*.genome" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."shepards" or (errorHandler.buildDepError "shepards"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."Unique" or (errorHandler.buildDepError "Unique"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
          ];
        buildable = true;
        modules = [
          "Hedgehog/Gen/Double"
          "Cole/Spec/Ledger/Core"
          "Cole/Spec/Ledger/Core/Generators"
          "Cole/Spec/Ledger/Core/Omniscient"
          "Cole/Spec/Ledger/Delegation"
          "Cole/Spec/Ledger/Delegation/Test"
          "Cole/Spec/Ledger/GlobalParams"
          "Cole/Spec/Ledger/Update"
          "Cole/Spec/Ledger/Update/Generators"
          "Cole/Spec/Ledger/Update/Test"
          "Cole/Spec/Ledger/UTxO"
          "Cole/Spec/Ledger/UTxO/Generators"
          "Cole/Spec/Ledger/Util"
          "Cole/Spec/Ledger/STS/UTXO"
          "Cole/Spec/Ledger/STS/UTXOW"
          "Cole/Spec/Ledger/STS/UTXOWS"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "cole-spec-ledger-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."Unique" or (errorHandler.buildDepError "Unique"))
            (hsPkgs."cole-spec-ledger" or (errorHandler.buildDepError "cole-spec-ledger"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            ];
          buildable = true;
          modules = [
            "Test/Cole/Spec/Ledger/Core/Generators/Properties"
            "Test/Cole/Spec/Ledger/Delegation/Examples"
            "Test/Cole/Spec/Ledger/Delegation/Properties"
            "Test/Cole/Spec/Ledger/AbstractSize/Properties"
            "Test/Cole/Spec/Ledger/Update/Examples"
            "Test/Cole/Spec/Ledger/Update/Properties"
            "Test/Cole/Spec/Ledger/Relation/Properties"
            "Test/Cole/Spec/Ledger/UTxO/Properties"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "8";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "8";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/cole/ledger/executable-spec; echo source root reset to \$sourceRoot";
    }