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
      identifier = { name = "sophie-spec-ledger-test"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@bcccoin.io";
      author = "GodXCoin Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Test helpers from sophie-spec-ledger exposed to other packages";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "cddl-files/sophie.cddl"
        "cddl-files/real/crypto.cddl"
        "cddl-files/mock/extras.cddl"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
          (hsPkgs."bcc-crypto-test" or (errorHandler.buildDepError "bcc-crypto-test"))
          (hsPkgs."bcc-crypto-wrapper" or (errorHandler.buildDepError "bcc-crypto-wrapper"))
          (hsPkgs."bcc-crypto" or (errorHandler.buildDepError "bcc-crypto"))
          (hsPkgs."bcc-ledger-cole" or (errorHandler.buildDepError "bcc-ledger-cole"))
          (hsPkgs."bcc-ledger-cole-test" or (errorHandler.buildDepError "bcc-ledger-cole-test"))
          (hsPkgs."bcc-ledger-core" or (errorHandler.buildDepError "bcc-ledger-core"))
          (hsPkgs."bcc-prelude-test" or (errorHandler.buildDepError "bcc-prelude-test"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."bcc-slotting" or (errorHandler.buildDepError "bcc-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."process-extras" or (errorHandler.buildDepError "process-extras"))
          (hsPkgs."zerepoch-ledger-api" or (errorHandler.buildDepError "zerepoch-ledger-api"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."sophie-spec-ledger" or (errorHandler.buildDepError "sophie-spec-ledger"))
          (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        modules = [
          "Test/Sophie/Spec/Ledger/Address/Bootstrap"
          "Test/Sophie/Spec/Ledger/Address/CompactAddr"
          "Test/Sophie/Spec/Ledger/ColeTranslation"
          "Test/Sophie/Spec/Ledger/Examples/Federation"
          "Test/Sophie/Spec/Ledger/Rules/TestDeleg"
          "Test/Sophie/Spec/Ledger/Rules/TestPool"
          "Test/Sophie/Spec/Ledger/Rules/TestPoolreap"
          "Test/Sophie/Spec/Ledger/SophieTranslation"
          "Test/Bcc/Crypto/VRF/Fake"
          "Test/Sophie/Spec/Ledger/BenchmarkFunctions"
          "Test/Sophie/Spec/Ledger/ConcreteCryptoTypes"
          "Test/Sophie/Spec/Ledger/Examples/Cast"
          "Test/Sophie/Spec/Ledger/Examples/Consensus"
          "Test/Sophie/Spec/Ledger/Generator/Block"
          "Test/Sophie/Spec/Ledger/Generator/Constants"
          "Test/Sophie/Spec/Ledger/Generator/Core"
          "Test/Sophie/Spec/Ledger/Generator/Delegation"
          "Test/Sophie/Spec/Ledger/Generator/Metadata"
          "Test/Sophie/Spec/Ledger/Generator/Presets"
          "Test/Sophie/Spec/Ledger/Generator/Trace/Chain"
          "Test/Sophie/Spec/Ledger/Generator/Trace/DCert"
          "Test/Sophie/Spec/Ledger/Generator/Trace/Ledger"
          "Test/Sophie/Spec/Ledger/Generator/Update"
          "Test/Sophie/Spec/Ledger/Generator/Utxo"
          "Test/Sophie/Spec/Ledger/Generator/EraGen"
          "Test/Sophie/Spec/Ledger/Generator/ScriptClass"
          "Test/Sophie/Spec/Ledger/Generator/SophieEraGen"
          "Test/Sophie/Spec/Ledger/Orphans"
          "Test/Sophie/Spec/Ledger/Rules/ClassifyTraces"
          "Test/Sophie/Spec/Ledger/Serialisation/CDDLUtils"
          "Test/Sophie/Spec/Ledger/Serialisation/Generators"
          "Test/Sophie/Spec/Ledger/Serialisation/EraIndepGenerators"
          "Test/Sophie/Spec/Ledger/Serialisation/Generators/Bootstrap"
          "Test/Sophie/Spec/Ledger/Serialisation/Generators/Genesis"
          "Test/Sophie/Spec/Ledger/Serialisation/GoldenUtils"
          "Test/Sophie/Spec/Ledger/Shrinkers"
          "Test/Sophie/Spec/Ledger/Utils"
          "Test/Sophie/Spec/Ledger/PropertyTests"
          "Test/Sophie/Spec/Ledger/Rules/TestChain"
          "Test/TestScenario"
          ];
        hsSourceDirs = [ "src" "test" ];
        };
      tests = {
        "sophie-spec-ledger-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
            (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
            (hsPkgs."bcc-crypto-praos" or (errorHandler.buildDepError "bcc-crypto-praos"))
            (hsPkgs."bcc-crypto-test" or (errorHandler.buildDepError "bcc-crypto-test"))
            (hsPkgs."bcc-crypto-wrapper" or (errorHandler.buildDepError "bcc-crypto-wrapper"))
            (hsPkgs."bcc-crypto" or (errorHandler.buildDepError "bcc-crypto"))
            (hsPkgs."bcc-ledger-cole-test" or (errorHandler.buildDepError "bcc-ledger-cole-test"))
            (hsPkgs."bcc-ledger-cole" or (errorHandler.buildDepError "bcc-ledger-cole"))
            (hsPkgs."bcc-ledger-core" or (errorHandler.buildDepError "bcc-ledger-core"))
            (hsPkgs."bcc-prelude-test" or (errorHandler.buildDepError "bcc-prelude-test"))
            (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
            (hsPkgs."bcc-slotting" or (errorHandler.buildDepError "bcc-slotting"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."sophie-spec-ledger-test" or (errorHandler.buildDepError "sophie-spec-ledger-test"))
            (hsPkgs."sophie-spec-ledger" or (errorHandler.buildDepError "sophie-spec-ledger"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            ];
          buildable = true;
          modules = [
            "Test/Control/Iterate/SetAlgebra"
            "Test/Sophie/Spec/Ledger/Address/Bootstrap"
            "Test/Sophie/Spec/Ledger/Address/CompactAddr"
            "Test/Sophie/Spec/Ledger/ColeTranslation"
            "Test/Sophie/Spec/Ledger/Examples"
            "Test/Sophie/Spec/Ledger/Examples/Cast"
            "Test/Sophie/Spec/Ledger/Examples/Combinators"
            "Test/Sophie/Spec/Ledger/Examples/EmptyBlock"
            "Test/Sophie/Spec/Ledger/Examples/Federation"
            "Test/Sophie/Spec/Ledger/Examples/Init"
            "Test/Sophie/Spec/Ledger/Examples/GenesisDelegation"
            "Test/Sophie/Spec/Ledger/Examples/NetworkID"
            "Test/Sophie/Spec/Ledger/Examples/Mir"
            "Test/Sophie/Spec/Ledger/Examples/MirTransfer"
            "Test/Sophie/Spec/Ledger/Examples/PoolLifetime"
            "Test/Sophie/Spec/Ledger/Examples/PoolReReg"
            "Test/Sophie/Spec/Ledger/Examples/TwoPools"
            "Test/Sophie/Spec/Ledger/Examples/Updates"
            "Test/Sophie/Spec/Ledger/Fees"
            "Test/Sophie/Spec/Ledger/MultiSigExamples"
            "Test/Sophie/Spec/Ledger/Pretty"
            "Test/Sophie/Spec/Ledger/PropertyTests"
            "Test/Sophie/Spec/Ledger/Rewards"
            "Test/Sophie/Spec/Ledger/Rules/ClassifyTraces"
            "Test/Sophie/Spec/Ledger/Rules/TestChain"
            "Test/Sophie/Spec/Ledger/Rules/TestDeleg"
            "Test/Sophie/Spec/Ledger/Rules/TestPool"
            "Test/Sophie/Spec/Ledger/Rules/TestPoolreap"
            "Test/Sophie/Spec/Ledger/SafeHash"
            "Test/Sophie/Spec/Ledger/Serialisation"
            "Test/Sophie/Spec/Ledger/Serialisation/CDDL"
            "Test/Sophie/Spec/Ledger/Serialisation/Golden/Address"
            "Test/Sophie/Spec/Ledger/Serialisation/Golden/Encoding"
            "Test/Sophie/Spec/Ledger/Serialisation/Golden/Genesis"
            "Test/Sophie/Spec/Ledger/Serialisation/Tripping/CBOR"
            "Test/Sophie/Spec/Ledger/Serialisation/Tripping/JSON"
            "Test/Sophie/Spec/Ledger/SophieTranslation"
            "Test/Sophie/Spec/Ledger/STSTests"
            "Test/Sophie/Spec/Ledger/UnitTests"
            "Test/TestScenario"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Tests.hs" ];
          };
        };
      benchmarks = {
        "mainbench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
            (hsPkgs."bcc-crypto-praos" or (errorHandler.buildDepError "bcc-crypto-praos"))
            (hsPkgs."bcc-ledger-core" or (errorHandler.buildDepError "bcc-ledger-core"))
            (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
            (hsPkgs."bcc-slotting" or (errorHandler.buildDepError "bcc-slotting"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."sophie-spec-ledger-test" or (errorHandler.buildDepError "sophie-spec-ledger-test"))
            (hsPkgs."sophie-spec-ledger" or (errorHandler.buildDepError "sophie-spec-ledger"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          modules = [
            "Bench/Control/Iterate/SetAlgebra/Bimap"
            "BenchUTxOAggregate"
            "BenchValidation"
            "Sophie/Spec/Ledger/Bench/Gen"
            "Sophie/Spec/Ledger/Bench/Rewards"
            "Test/Sophie/Spec/Ledger/Examples/Cast"
            ];
          hsSourceDirs = [ "bench" "test" ];
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
    postUnpack = "sourceRoot+=/sophie/chain-and-ledger/sophie-spec-ledger-test; echo source root reset to \$sourceRoot";
    }