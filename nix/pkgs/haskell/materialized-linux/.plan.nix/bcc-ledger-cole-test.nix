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
      identifier = { name = "bcc-ledger-cole-test"; version = "1.3.0"; };
      license = "Apache-2.0";
      copyright = "2018 The Blockchain Co.";
      maintainer = "operations@blockchain-company.io";
      author = "The Blockchain Co.";
      homepage = "";
      url = "";
      synopsis = "Test helpers from bcc-ledger exposed to other packages";
      description = "Test helpers from bcc-ledger exposed to other packages";
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
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."bcc-binary-test" or (errorHandler.buildDepError "bcc-binary-test"))
          (hsPkgs."bcc-ledger-cole" or (errorHandler.buildDepError "bcc-ledger-cole"))
          (hsPkgs."bcc-crypto" or (errorHandler.buildDepError "bcc-crypto"))
          (hsPkgs."bcc-crypto-test" or (errorHandler.buildDepError "bcc-crypto-test"))
          (hsPkgs."bcc-crypto-wrapper" or (errorHandler.buildDepError "bcc-crypto-wrapper"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."bcc-prelude-test" or (errorHandler.buildDepError "bcc-prelude-test"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cole-spec-chain" or (errorHandler.buildDepError "cole-spec-chain"))
          (hsPkgs."cole-spec-ledger" or (errorHandler.buildDepError "cole-spec-ledger"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."generic-monoid" or (errorHandler.buildDepError "generic-monoid"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        modules = [
          "Test/Bcc/Chain/Block/CBOR"
          "Test/Bcc/Chain/Block/Gen"
          "Test/Bcc/Chain/Block/Model"
          "Test/Bcc/Chain/Block/Model/Examples"
          "Test/Bcc/Chain/Block/Validation"
          "Test/Bcc/Chain/Block/ValidationMode"
          "Test/Bcc/Chain/Cole/API"
          "Test/Bcc/Chain/Buildable"
          "Test/Bcc/Chain/Common/Address"
          "Test/Bcc/Chain/Common/CBOR"
          "Test/Bcc/Chain/Common/Compact"
          "Test/Bcc/Chain/Common/Example"
          "Test/Bcc/Chain/Common/Gen"
          "Test/Bcc/Chain/Common/Entropic"
          "Test/Bcc/Chain/Config"
          "Test/Bcc/Chain/Delegation/CBOR"
          "Test/Bcc/Chain/Delegation/Certificate"
          "Test/Bcc/Chain/Delegation/Example"
          "Test/Bcc/Chain/Delegation/Gen"
          "Test/Bcc/Chain/Delegation/Model"
          "Test/Bcc/Chain/Elaboration/Block"
          "Test/Bcc/Chain/Elaboration/Delegation"
          "Test/Bcc/Chain/Elaboration/Keys"
          "Test/Bcc/Chain/Elaboration/Update"
          "Test/Bcc/Chain/Elaboration/UTxO"
          "Test/Bcc/Chain/Epoch/File"
          "Test/Bcc/Chain/Genesis/CBOR"
          "Test/Bcc/Chain/Genesis/Dummy"
          "Test/Bcc/Chain/Genesis/Example"
          "Test/Bcc/Chain/Genesis/Gen"
          "Test/Bcc/Chain/Genesis/Json"
          "Test/Bcc/Chain/MempoolPayload/CBOR"
          "Test/Bcc/Chain/MempoolPayload/Example"
          "Test/Bcc/Chain/MempoolPayload/Gen"
          "Test/Bcc/Chain/Ssc/CBOR"
          "Test/Bcc/Chain/Slotting/CBOR"
          "Test/Bcc/Chain/Slotting/Example"
          "Test/Bcc/Chain/Slotting/Gen"
          "Test/Bcc/Chain/Slotting/Properties"
          "Test/Bcc/Chain/UTxO/CBOR"
          "Test/Bcc/Chain/UTxO/Compact"
          "Test/Bcc/Chain/UTxO/Example"
          "Test/Bcc/Chain/UTxO/Gen"
          "Test/Bcc/Chain/UTxO/Model"
          "Test/Bcc/Chain/UTxO/ValidationMode"
          "Test/Bcc/Chain/Update/CBOR"
          "Test/Bcc/Chain/Update/Example"
          "Test/Bcc/Chain/Update/Gen"
          "Test/Bcc/Chain/Update/Properties"
          "Test/Bcc/Mirror"
          "Test/Options"
          ];
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
    postUnpack = "sourceRoot+=/cole/ledger/impl/test; echo source root reset to \$sourceRoot";
    }