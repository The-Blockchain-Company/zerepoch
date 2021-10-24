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
    flags = { development = false; test-normal-form = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "bcc-ledger-cole"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2018 The Blockchain Co.";
      maintainer = "operations@bcccoin.io";
      author = "The Blockchain Co.";
      homepage = "";
      url = "";
      synopsis = "The blockchain layer of Bcc during the Cole era";
      description = "The blockchain layer of Bcc during the Cole era";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."canonical-json" or (errorHandler.buildDepError "canonical-json"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."bcc-crypto" or (errorHandler.buildDepError "bcc-crypto"))
          (hsPkgs."bcc-crypto-wrapper" or (errorHandler.buildDepError "bcc-crypto-wrapper"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."digest" or (errorHandler.buildDepError "digest"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."streaming-binary" or (errorHandler.buildDepError "streaming-binary"))
          (hsPkgs."streaming-bytestring" or (errorHandler.buildDepError "streaming-bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        modules = [
          "Bcc/Chain/Block/Block"
          "Bcc/Chain/Block/Body"
          "Bcc/Chain/Block/Boundary"
          "Bcc/Chain/Block/Header"
          "Bcc/Chain/Block/Proof"
          "Bcc/Chain/Block/Validation"
          "Bcc/Chain/Block/ValidationMode"
          "Bcc/Chain/Cole/API/Common"
          "Bcc/Chain/Cole/API/Mempool"
          "Bcc/Chain/Cole/API/Protocol"
          "Bcc/Chain/Cole/API/Validation"
          "Bcc/Chain/Common/AddrAttributes"
          "Bcc/Chain/Common/AddrSpendingData"
          "Bcc/Chain/Common/Address"
          "Bcc/Chain/Common/AddressHash"
          "Bcc/Chain/Common/Attributes"
          "Bcc/Chain/Common/BlockCount"
          "Bcc/Chain/Common/CBOR"
          "Bcc/Chain/Common/ChainDifficulty"
          "Bcc/Chain/Common/Compact"
          "Bcc/Chain/Common/KeyHash"
          "Bcc/Chain/Common/Entropic"
          "Bcc/Chain/Common/EntropicPortion"
          "Bcc/Chain/Common/Merkle"
          "Bcc/Chain/Common/NetworkMagic"
          "Bcc/Chain/Common/TxFeePolicy"
          "Bcc/Chain/Common/TxSizeLinear"
          "Bcc/Chain/Delegation/Certificate"
          "Bcc/Chain/Delegation/Map"
          "Bcc/Chain/Delegation/Payload"
          "Bcc/Chain/Genesis/AvvmBalances"
          "Bcc/Chain/Genesis/Config"
          "Bcc/Chain/Genesis/Data"
          "Bcc/Chain/Genesis/Delegation"
          "Bcc/Chain/Genesis/Generate"
          "Bcc/Chain/Genesis/Hash"
          "Bcc/Chain/Genesis/Initializer"
          "Bcc/Chain/Genesis/KeyHashes"
          "Bcc/Chain/Genesis/NonAvvmBalances"
          "Bcc/Chain/Genesis/Spec"
          "Bcc/Chain/Slotting/EpochAndSlotCount"
          "Bcc/Chain/Slotting/EpochNumber"
          "Bcc/Chain/Slotting/EpochSlots"
          "Bcc/Chain/Slotting/SlotCount"
          "Bcc/Chain/Slotting/SlotNumber"
          "Bcc/Chain/UTxO/Compact"
          "Bcc/Chain/UTxO/GenesisUTxO"
          "Bcc/Chain/UTxO/Tx"
          "Bcc/Chain/UTxO/TxAux"
          "Bcc/Chain/UTxO/TxPayload"
          "Bcc/Chain/UTxO/UTxOConfiguration"
          "Bcc/Chain/UTxO/TxProof"
          "Bcc/Chain/UTxO/TxWitness"
          "Bcc/Chain/UTxO/ValidationMode"
          "Bcc/Chain/Update/ApplicationName"
          "Bcc/Chain/Update/InstallerHash"
          "Bcc/Chain/Update/Payload"
          "Bcc/Chain/Update/Proof"
          "Bcc/Chain/Update/ProtocolParameters"
          "Bcc/Chain/Update/ProtocolParametersUpdate"
          "Bcc/Chain/Update/ProtocolVersion"
          "Bcc/Chain/Update/SoftforkRule"
          "Bcc/Chain/Update/SoftwareVersion"
          "Bcc/Chain/Update/SystemTag"
          "Bcc/Chain/Update/Validation/Interface/ProtocolVersionBump"
          "Bcc/Chain/Block"
          "Bcc/Chain/Cole/API"
          "Bcc/Chain/Common"
          "Bcc/Chain/Constants"
          "Bcc/Chain/Delegation"
          "Bcc/Chain/Delegation/Validation/Activation"
          "Bcc/Chain/Delegation/Validation/Interface"
          "Bcc/Chain/Delegation/Validation/Scheduling"
          "Bcc/Chain/Epoch/File"
          "Bcc/Chain/Epoch/Validation"
          "Bcc/Chain/Genesis"
          "Bcc/Chain/MempoolPayload"
          "Bcc/Chain/ProtocolConstants"
          "Bcc/Chain/Slotting"
          "Bcc/Chain/Ssc"
          "Bcc/Chain/UTxO"
          "Bcc/Chain/UTxO/UTxO"
          "Bcc/Chain/UTxO/Validation"
          "Bcc/Chain/Update"
          "Bcc/Chain/Update/Proposal"
          "Bcc/Chain/Update/Validation/Endorsement"
          "Bcc/Chain/Update/Validation/Interface"
          "Bcc/Chain/Update/Validation/Registration"
          "Bcc/Chain/Update/Validation/Voting"
          "Bcc/Chain/Update/Vote"
          "Bcc/Chain/ValidationMode"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "bcc-ledger-cole-test" = {
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
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
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
            "Test/Bcc/Chain/Block/Size"
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
          hsSourceDirs = [ "test" ];
          mainPath = [ "test.hs" ];
          };
        "epoch-validation-normal-form-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
            (hsPkgs."bcc-ledger" or (errorHandler.buildDepError "bcc-ledger"))
            (hsPkgs."bcc-crypto-wrapper" or (errorHandler.buildDepError "bcc-crypto-wrapper"))
            (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
            (hsPkgs."bcc-prelude-test" or (errorHandler.buildDepError "bcc-prelude-test"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."silently" or (errorHandler.buildDepError "silently"))
            (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            ];
          buildable = if !flags.test-normal-form then false else true;
          modules = [
            "Test/Bcc/Chain/Block/Validation"
            "Test/Bcc/Chain/Config"
            "Test/Bcc/Mirror"
            "Test/Options"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "NormalFormTest.hs" ];
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
    postUnpack = "sourceRoot+=/cole/ledger/impl; echo source root reset to \$sourceRoot";
    }