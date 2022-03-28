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
    flags = { asserts = false; };
    package = {
      specVersion = "1.10";
      identifier = {
        name = "shardagnostic-consensus-bcc";
        version = "0.1.0.0";
        };
      license = "Apache-2.0";
      copyright = "2021 The-Blockchain-Company ";
      maintainer = "operations@blockchain-company.io";
      author = "The Blockchain Co. Engineering Team";
      homepage = "";
      url = "";
      synopsis = "The instantation of the Shardagnostic consensus layer used by Bcc";
      description = "";
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
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
          (hsPkgs."bcc-ledger-aurum" or (errorHandler.buildDepError "bcc-ledger-aurum"))
          (hsPkgs."bcc-ledger-cole" or (errorHandler.buildDepError "bcc-ledger-cole"))
          (hsPkgs."bcc-ledger-core" or (errorHandler.buildDepError "bcc-ledger-core"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."bcc-slotting" or (errorHandler.buildDepError "bcc-slotting"))
          (hsPkgs."sophie-spec-ledger" or (errorHandler.buildDepError "sophie-spec-ledger"))
          (hsPkgs."bcc-ledger-sophie-ma" or (errorHandler.buildDepError "bcc-ledger-sophie-ma"))
          (hsPkgs."shardagnostic-network" or (errorHandler.buildDepError "shardagnostic-network"))
          (hsPkgs."shardagnostic-consensus" or (errorHandler.buildDepError "shardagnostic-consensus"))
          (hsPkgs."shardagnostic-consensus-cole" or (errorHandler.buildDepError "shardagnostic-consensus-cole"))
          (hsPkgs."shardagnostic-consensus-sophie" or (errorHandler.buildDepError "shardagnostic-consensus-sophie"))
          ];
        buildable = true;
        modules = [
          "Shardagnostic/Consensus/Bcc"
          "Shardagnostic/Consensus/Bcc/Block"
          "Shardagnostic/Consensus/Bcc/ColeHFC"
          "Shardagnostic/Consensus/Bcc/Condense"
          "Shardagnostic/Consensus/Bcc/CanHardFork"
          "Shardagnostic/Consensus/Bcc/Node"
          "Shardagnostic/Consensus/Bcc/SophieBased"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "db-analyser" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
            (hsPkgs."bcc-crypto-wrapper" or (errorHandler.buildDepError "bcc-crypto-wrapper"))
            (hsPkgs."bcc-ledger-aurum" or (errorHandler.buildDepError "bcc-ledger-aurum"))
            (hsPkgs."bcc-ledger-cole" or (errorHandler.buildDepError "bcc-ledger-cole"))
            (hsPkgs."bcc-ledger-core" or (errorHandler.buildDepError "bcc-ledger-core"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."sophie-spec-ledger" or (errorHandler.buildDepError "sophie-spec-ledger"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."shardagnostic-consensus" or (errorHandler.buildDepError "shardagnostic-consensus"))
            (hsPkgs."shardagnostic-consensus-cole" or (errorHandler.buildDepError "shardagnostic-consensus-cole"))
            (hsPkgs."shardagnostic-consensus-bcc" or (errorHandler.buildDepError "shardagnostic-consensus-bcc"))
            (hsPkgs."shardagnostic-consensus-sophie" or (errorHandler.buildDepError "shardagnostic-consensus-sophie"))
            (hsPkgs."shardagnostic-network" or (errorHandler.buildDepError "shardagnostic-network"))
            ];
          buildable = true;
          modules = [
            "Analysis"
            "Block/Cole"
            "Block/Bcc"
            "Block/Sophie"
            "HasAnalysis"
            ];
          hsSourceDirs = [ "tools/db-analyser" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "6";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "6";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/shardagnostic-consensus-bcc; echo source root reset to \$sourceRoot";
    }