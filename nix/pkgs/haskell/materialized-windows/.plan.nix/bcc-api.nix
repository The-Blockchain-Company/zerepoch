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
      identifier = { name = "bcc-api"; version = "1.29.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@bcccoin.io";
      author = "GodXCoin";
      homepage = "";
      url = "";
      synopsis = "";
      description = "The bcc api";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" "ChangeLog.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."bcc-crypto" or (errorHandler.buildDepError "bcc-crypto"))
          (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
          (hsPkgs."bcc-crypto-wrapper" or (errorHandler.buildDepError "bcc-crypto-wrapper"))
          (hsPkgs."bcc-ledger-aurum" or (errorHandler.buildDepError "bcc-ledger-aurum"))
          (hsPkgs."bcc-ledger-cole" or (errorHandler.buildDepError "bcc-ledger-cole"))
          (hsPkgs."bcc-ledger-core" or (errorHandler.buildDepError "bcc-ledger-core"))
          (hsPkgs."bcc-ledger-sophie-ma" or (errorHandler.buildDepError "bcc-ledger-sophie-ma"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."bcc-slotting" or (errorHandler.buildDepError "bcc-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."shardagnostic-consensus" or (errorHandler.buildDepError "shardagnostic-consensus"))
          (hsPkgs."shardagnostic-consensus-cole" or (errorHandler.buildDepError "shardagnostic-consensus-cole"))
          (hsPkgs."shardagnostic-consensus-bcc" or (errorHandler.buildDepError "shardagnostic-consensus-bcc"))
          (hsPkgs."shardagnostic-consensus-sophie" or (errorHandler.buildDepError "shardagnostic-consensus-sophie"))
          (hsPkgs."shardagnostic-network" or (errorHandler.buildDepError "shardagnostic-network"))
          (hsPkgs."shardagnostic-network-framework" or (errorHandler.buildDepError "shardagnostic-network-framework"))
          (hsPkgs."zerepoch-ledger-api" or (errorHandler.buildDepError "zerepoch-ledger-api"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."sophie-spec-ledger" or (errorHandler.buildDepError "sophie-spec-ledger"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-except" or (errorHandler.buildDepError "transformers-except"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          ];
        buildable = true;
        modules = [
          "Bcc/Api/Address"
          "Bcc/Api/Block"
          "Bcc/Api/Certificate"
          "Bcc/Api/Eras"
          "Bcc/Api/Error"
          "Bcc/Api/Fees"
          "Bcc/Api/GenesisParameters"
          "Bcc/Api/Hash"
          "Bcc/Api/HasTypeProxy"
          "Bcc/Api/IPC"
          "Bcc/Api/IPC/Monad"
          "Bcc/Api/Json"
          "Bcc/Api/Key"
          "Bcc/Api/KeysCole"
          "Bcc/Api/KeysPraos"
          "Bcc/Api/KeysSophie"
          "Bcc/Api/LedgerState"
          "Bcc/Api/Modes"
          "Bcc/Api/NetworkId"
          "Bcc/Api/OperationalCertificate"
          "Bcc/Api/ProtocolParameters"
          "Bcc/Api/Query"
          "Bcc/Api/Script"
          "Bcc/Api/ScriptData"
          "Bcc/Api/SerialiseBech32"
          "Bcc/Api/SerialiseCBOR"
          "Bcc/Api/SerialiseJSON"
          "Bcc/Api/SerialiseRaw"
          "Bcc/Api/SerialiseTextEnvelope"
          "Bcc/Api/SerialiseUsing"
          "Bcc/Api/Sophie/Genesis"
          "Bcc/Api/SpecialCole"
          "Bcc/Api/StakePoolMetadata"
          "Bcc/Api/Tx"
          "Bcc/Api/TxBody"
          "Bcc/Api/TxInMode"
          "Bcc/Api/TxMetadata"
          "Bcc/Api/TxSubmit/ErrorRender"
          "Bcc/Api/TxSubmit/Types"
          "Bcc/Api/Utils"
          "Bcc/Api/Value"
          "Bcc/Api"
          "Bcc/Api/Cole"
          "Bcc/Api/Crypto/Ed25519Bip32"
          "Bcc/Api/Sophie"
          "Bcc/Api/ChainSync/Client"
          "Bcc/Api/ChainSync/ClientPipelined"
          "Bcc/Api/Orphans"
          "Bcc/Api/Protocol/Cole"
          "Bcc/Api/Protocol/Bcc"
          "Bcc/Api/Protocol/Sophie"
          "Bcc/Api/Protocol/Types"
          ];
        hsSourceDirs = [ "src" ];
        };
      sublibs = {
        "gen" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bcc-api" or (errorHandler.buildDepError "bcc-api"))
            (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
            (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
            (hsPkgs."bcc-crypto-test" or (errorHandler.buildDepError "bcc-crypto-test"))
            (hsPkgs."bcc-ledger-aurum" or (errorHandler.buildDepError "bcc-ledger-aurum"))
            (hsPkgs."bcc-ledger-cole-test" or (errorHandler.buildDepError "bcc-ledger-cole-test"))
            (hsPkgs."bcc-ledger-core" or (errorHandler.buildDepError "bcc-ledger-core"))
            (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."zerepoch-ledger-api" or (errorHandler.buildDepError "zerepoch-ledger-api"))
            (hsPkgs."sophie-spec-ledger" or (errorHandler.buildDepError "sophie-spec-ledger"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [
            "Gen/Bcc/Api"
            "Gen/Bcc/Api/Metadata"
            "Gen/Bcc/Api/Typed"
            "Gen/Bcc/Crypto/Seed"
            "Gen/Hedgehog/Roundtrip/Bech32"
            "Gen/Hedgehog/Roundtrip/CBOR"
            "Gen/Tasty/Hedgehog/Group"
            ];
          hsSourceDirs = [ "gen" ];
          };
        };
      tests = {
        "bcc-api-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bcc-api" or (errorHandler.buildDepError "bcc-api"))
            (hsPkgs."bcc-api".components.sublibs.gen or (errorHandler.buildDepError "bcc-api:gen"))
            (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
            (hsPkgs."bcc-crypto" or (errorHandler.buildDepError "bcc-crypto"))
            (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
            (hsPkgs."bcc-crypto-test" or (errorHandler.buildDepError "bcc-crypto-test"))
            (hsPkgs."bcc-crypto-tests" or (errorHandler.buildDepError "bcc-crypto-tests"))
            (hsPkgs."bcc-ledger-core" or (errorHandler.buildDepError "bcc-ledger-core"))
            (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
            (hsPkgs."bcc-slotting" or (errorHandler.buildDepError "bcc-slotting"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-extras" or (errorHandler.buildDepError "hedgehog-extras"))
            (hsPkgs."shardagnostic-consensus" or (errorHandler.buildDepError "shardagnostic-consensus"))
            (hsPkgs."shardagnostic-consensus-sophie" or (errorHandler.buildDepError "shardagnostic-consensus-sophie"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."sophie-spec-ledger" or (errorHandler.buildDepError "sophie-spec-ledger"))
            (hsPkgs."sophie-spec-ledger-test" or (errorHandler.buildDepError "sophie-spec-ledger-test"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          modules = [
            "Test/Bcc/Api/Crypto"
            "Test/Bcc/Api/Genesis"
            "Test/Bcc/Api/Json"
            "Test/Bcc/Api/KeysCole"
            "Test/Bcc/Api/Ledger"
            "Test/Bcc/Api/Metadata"
            "Test/Bcc/Api/Typed/Bech32"
            "Test/Bcc/Api/Typed/CBOR"
            "Test/Bcc/Api/Typed/Envelope"
            "Test/Bcc/Api/Typed/JSON"
            "Test/Bcc/Api/Typed/Ord"
            "Test/Bcc/Api/Typed/Orphans"
            "Test/Bcc/Api/Typed/RawBytes"
            "Test/Bcc/Api/Typed/Script"
            "Test/Bcc/Api/Typed/Value"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "bcc-api-test.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "9";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "9";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/bcc-api; echo source root reset to \$sourceRoot";
    }