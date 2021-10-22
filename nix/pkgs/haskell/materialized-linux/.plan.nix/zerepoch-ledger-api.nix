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
      specVersion = "2.2";
      identifier = { name = "zerepoch-ledger-api"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@bcccoin.io";
      author = "Michael Peyton Jones, Jann Mueller";
      homepage = "";
      url = "";
      synopsis = "Interface to the Zerepoch ledger for the Bcc ledger.";
      description = "Interface to the Zerepoch scripting support for the Bcc ledger.";
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
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          ];
        buildable = true;
        modules = [
          "Data/Aeson/Extras"
          "Data/Either/Extras"
          "Data/Text/Prettyprint/Doc/Extras"
          "Zerepoch/V1/Ledger/Address"
          "Zerepoch/V1/Ledger/Bcc"
          "Zerepoch/V1/Ledger/Api"
          "Zerepoch/V1/Ledger/Bytes"
          "Zerepoch/V1/Ledger/Contexts"
          "Zerepoch/V1/Ledger/Credential"
          "Zerepoch/V1/Ledger/Crypto"
          "Zerepoch/V1/Ledger/DCert"
          "Zerepoch/V1/Ledger/Examples"
          "Zerepoch/V1/Ledger/Interval"
          "Zerepoch/V1/Ledger/Orphans"
          "Zerepoch/V1/Ledger/Scripts"
          "Zerepoch/V1/Ledger/Slot"
          "Zerepoch/V1/Ledger/Tx"
          "Zerepoch/V1/Ledger/TxId"
          "Zerepoch/V1/Ledger/Time"
          "Zerepoch/V1/Ledger/Value"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "zerepoch-ledger-api-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."zerepoch-ledger-api" or (errorHandler.buildDepError "zerepoch-ledger-api"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          modules = [ "Spec/Interval" "Spec/Time" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../zerepoch-ledger-api; }