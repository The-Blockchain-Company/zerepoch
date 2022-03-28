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
      specVersion = "3.0";
      identifier = { name = "bcc-ledger-aurum"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2021 The-Blockchain-Company ";
      maintainer = "dev@blockchain-company.io";
      author = "The Blockchain Co. Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Bcc ledger introducing Zerepoch Core";
      description = "This package builds upon the Jen ledger with support for extended UTxO\nvia Zerepoch Core.";
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
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base-deriving-via" or (errorHandler.buildDepError "base-deriving-via"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
          (hsPkgs."bcc-ledger-core" or (errorHandler.buildDepError "bcc-ledger-core"))
          (hsPkgs."bcc-ledger-sophie-ma" or (errorHandler.buildDepError "bcc-ledger-sophie-ma"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."bcc-slotting" or (errorHandler.buildDepError "bcc-slotting"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."measures" or (errorHandler.buildDepError "measures"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."zerepoch-ledger-api" or (errorHandler.buildDepError "zerepoch-ledger-api"))
          (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
          (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."sophie-spec-ledger" or (errorHandler.buildDepError "sophie-spec-ledger"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          ];
        buildable = true;
        modules = [
          "Bcc/Ledger/Aurum"
          "Bcc/Ledger/Aurum/Data"
          "Bcc/Ledger/Aurum/Genesis"
          "Bcc/Ledger/Aurum/Language"
          "Bcc/Ledger/Aurum/ZerepochScriptApi"
          "Bcc/Ledger/Aurum/PParams"
          "Bcc/Ledger/Aurum/Rules/Bbody"
          "Bcc/Ledger/Aurum/Rules/Ledger"
          "Bcc/Ledger/Aurum/Rules/Utxo"
          "Bcc/Ledger/Aurum/Rules/Utxos"
          "Bcc/Ledger/Aurum/Rules/Utxow"
          "Bcc/Ledger/Aurum/Scripts"
          "Bcc/Ledger/Aurum/Tools"
          "Bcc/Ledger/Aurum/Translation"
          "Bcc/Ledger/Aurum/Tx"
          "Bcc/Ledger/Aurum/TxBody"
          "Bcc/Ledger/Aurum/TxInfo"
          "Bcc/Ledger/Aurum/TxSeq"
          "Bcc/Ledger/Aurum/TxWitness"
          "Bcc/Ledger/DescribeEras"
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
    postUnpack = "sourceRoot+=/aurum/impl; echo source root reset to \$sourceRoot";
    }