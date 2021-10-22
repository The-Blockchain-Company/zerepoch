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
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "bcc-ledger-core"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2021 The-Blockchain-Company ";
      maintainer = "formal.methods@bcccoin.io";
      author = "GodXCoin Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Core components of Bcc ledgers from the Sophie release on.";
      description = "Bcc ledgers from the Sophie release onwards share a core basis rooted in\nthe Sophie ledger specification. This package abstracts a number of components\nwhich we expect to be shared amongst all future ledgers implemented around this base.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
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
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
          (hsPkgs."bcc-crypto-praos" or (errorHandler.buildDepError "bcc-crypto-praos"))
          (hsPkgs."bcc-crypto-wrapper" or (errorHandler.buildDepError "bcc-crypto-wrapper"))
          (hsPkgs."bcc-ledger-cole" or (errorHandler.buildDepError "bcc-ledger-cole"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."bcc-slotting" or (errorHandler.buildDepError "bcc-slotting"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."partial-order" or (errorHandler.buildDepError "partial-order"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."sophie-spec-non-integral" or (errorHandler.buildDepError "sophie-spec-non-integral"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        modules = [
          "Bcc/Ledger/Address"
          "Bcc/Ledger/AuxiliaryData"
          "Bcc/Ledger/BaseTypes"
          "Bcc/Ledger/Coin"
          "Bcc/Ledger/Compactible"
          "Bcc/Ledger/Core"
          "Bcc/Ledger/Credential"
          "Bcc/Ledger/Crypto"
          "Bcc/Ledger/Era"
          "Bcc/Ledger/Keys"
          "Bcc/Ledger/Hashes"
          "Bcc/Ledger/Rules/ValidationMode"
          "Bcc/Ledger/SafeHash"
          "Bcc/Ledger/Serialization"
          "Bcc/Ledger/Slot"
          "Bcc/Ledger/Val"
          ];
        hsSourceDirs = [ "src" ];
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
    postUnpack = "sourceRoot+=/bcc-ledger-core; echo source root reset to \$sourceRoot";
    }