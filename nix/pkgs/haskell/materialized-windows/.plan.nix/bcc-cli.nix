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
    flags = { unexpected_thunks = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "bcc-cli"; version = "1.99.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@blockchain-company.io";
      author = "The Blockchain Co.";
      homepage = "";
      url = "";
      synopsis = "";
      description = "The Bcc command-line interface.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bcc-api" or (errorHandler.buildDepError "bcc-api"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."bcc-config" or (errorHandler.buildDepError "bcc-config"))
          (hsPkgs."bcc-crypto" or (errorHandler.buildDepError "bcc-crypto"))
          (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
          (hsPkgs."bcc-crypto-wrapper" or (errorHandler.buildDepError "bcc-crypto-wrapper"))
          (hsPkgs."bcc-ledger-aurum" or (errorHandler.buildDepError "bcc-ledger-aurum"))
          (hsPkgs."bcc-ledger-cole" or (errorHandler.buildDepError "bcc-ledger-cole"))
          (hsPkgs."bcc-ledger-core" or (errorHandler.buildDepError "bcc-ledger-core"))
          (hsPkgs."bcc-ledger-sophie-ma" or (errorHandler.buildDepError "bcc-ledger-sophie-ma"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."bcc-slotting" or (errorHandler.buildDepError "bcc-slotting"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."optparse-applicative-fork" or (errorHandler.buildDepError "optparse-applicative-fork"))
          (hsPkgs."shardagnostic-consensus" or (errorHandler.buildDepError "shardagnostic-consensus"))
          (hsPkgs."shardagnostic-consensus-cole" or (errorHandler.buildDepError "shardagnostic-consensus-cole"))
          (hsPkgs."shardagnostic-consensus-bcc" or (errorHandler.buildDepError "shardagnostic-consensus-bcc"))
          (hsPkgs."shardagnostic-consensus-sophie" or (errorHandler.buildDepError "shardagnostic-consensus-sophie"))
          (hsPkgs."shardagnostic-network" or (errorHandler.buildDepError "shardagnostic-network"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."zerepoch-ledger-api" or (errorHandler.buildDepError "zerepoch-ledger-api"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."sophie-spec-ledger" or (errorHandler.buildDepError "sophie-spec-ledger"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."split" or (errorHandler.buildDepError "split"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-except" or (errorHandler.buildDepError "transformers-except"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
        buildable = true;
        modules = [
          "Paths_bcc_cli"
          "Bcc/CLI/Helpers"
          "Bcc/CLI/Parsers"
          "Bcc/CLI/Render"
          "Bcc/CLI/Run"
          "Bcc/CLI/Run/Friendly"
          "Bcc/CLI/Types"
          "Bcc/CLI/Environment"
          "Bcc/CLI/Cole/Commands"
          "Bcc/CLI/Cole/Parsers"
          "Bcc/CLI/Cole/Run"
          "Bcc/CLI/Cole/Delegation"
          "Bcc/CLI/Cole/Genesis"
          "Bcc/CLI/Cole/Key"
          "Bcc/CLI/Cole/Legacy"
          "Bcc/CLI/Cole/Tx"
          "Bcc/CLI/Cole/Query"
          "Bcc/CLI/Cole/UpdateProposal"
          "Bcc/CLI/Cole/Vote"
          "Bcc/CLI/Sophie/Commands"
          "Bcc/CLI/Sophie/Key"
          "Bcc/CLI/Sophie/Orphans"
          "Bcc/CLI/Sophie/Output"
          "Bcc/CLI/Sophie/Parsers"
          "Bcc/CLI/Sophie/Run"
          "Bcc/CLI/Sophie/Run/Address"
          "Bcc/CLI/Sophie/Run/Address/Info"
          "Bcc/CLI/Sophie/Run/Genesis"
          "Bcc/CLI/Sophie/Run/Governance"
          "Bcc/CLI/Sophie/Run/Key"
          "Bcc/CLI/Sophie/Run/Node"
          "Bcc/CLI/Sophie/Run/Pool"
          "Bcc/CLI/Sophie/Run/Query"
          "Bcc/CLI/Sophie/Run/StakeAddress"
          "Bcc/CLI/Sophie/Run/TextView"
          "Bcc/CLI/Sophie/Run/Transaction"
          "Bcc/CLI/Sophie/Script"
          "Bcc/CLI/Jen/ValueParser"
          "Bcc/CLI/TopHandler"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "bcc-cli" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bcc-cli" or (errorHandler.buildDepError "bcc-cli"))
            (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
            (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
            (hsPkgs."optparse-applicative-fork" or (errorHandler.buildDepError "optparse-applicative-fork"))
            (hsPkgs."transformers-except" or (errorHandler.buildDepError "transformers-except"))
            ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [
            "bcc-cli.hs"
            ] ++ (pkgs.lib).optional (!system.isWindows) "";
          };
        };
      tests = {
        "bcc-cli-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bcc-api" or (errorHandler.buildDepError "bcc-api"))
            (hsPkgs."bcc-api".components.sublibs.gen or (errorHandler.buildDepError "bcc-api:gen"))
            (hsPkgs."bcc-cli" or (errorHandler.buildDepError "bcc-cli"))
            (hsPkgs."bcc-node" or (errorHandler.buildDepError "bcc-node"))
            (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
            (hsPkgs."bcc-slotting" or (errorHandler.buildDepError "bcc-slotting"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-extras" or (errorHandler.buildDepError "hedgehog-extras"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [
            "Test/Cli/FilePermissions"
            "Test/Cli/ITN"
            "Test/Cli/MultiAssetParsing"
            "Test/Cli/Pioneers/Exercise1"
            "Test/Cli/Pioneers/Exercise2"
            "Test/Cli/Pioneers/Exercise3"
            "Test/Cli/Pioneers/Exercise4"
            "Test/Cli/Pioneers/Exercise5"
            "Test/Cli/Pioneers/Exercise6"
            "Test/Cli/Sophie/Run/Query"
            "Test/OptParse"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "bcc-cli-test.hs" ];
          };
        "bcc-cli-golden" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bcc-api" or (errorHandler.buildDepError "bcc-api"))
            (hsPkgs."bcc-cli" or (errorHandler.buildDepError "bcc-cli"))
            (hsPkgs."bcc-crypto-wrapper" or (errorHandler.buildDepError "bcc-crypto-wrapper"))
            (hsPkgs."bcc-ledger-cole" or (errorHandler.buildDepError "bcc-ledger-cole"))
            (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."Diff" or (errorHandler.buildDepError "Diff"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-extras" or (errorHandler.buildDepError "hedgehog-extras"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.bcc-cli.components.exes.bcc-cli or (pkgs.buildPackages.bcc-cli or (errorHandler.buildToolDepError "bcc-cli:bcc-cli")))
            ];
          buildable = true;
          modules = [
            "Test/Golden/Cole/SigningKeys"
            "Test/Golden/Cole/Tx"
            "Test/Golden/Cole/TxBody"
            "Test/Golden/Cole/UpdateProposal"
            "Test/Golden/Cole/Vote"
            "Test/Golden/Cole/Witness"
            "Test/Golden/Sophie"
            "Test/Golden/Sophie/Address/Build"
            "Test/Golden/Sophie/Address/Info"
            "Test/Golden/Sophie/Address/KeyGen"
            "Test/Golden/Sophie/Genesis/Create"
            "Test/Golden/Sophie/Genesis/InitialTxIn"
            "Test/Golden/Sophie/Genesis/KeyGenDelegate"
            "Test/Golden/Sophie/Genesis/KeyGenGenesis"
            "Test/Golden/Sophie/Genesis/KeyGenUtxo"
            "Test/Golden/Sophie/Genesis/KeyHash"
            "Test/Golden/Sophie/Key/ConvertBccAddressKey"
            "Test/Golden/Sophie/Metadata/StakePoolMetadata"
            "Test/Golden/Sophie/MultiSig/Address"
            "Test/Golden/Sophie/Node/IssueOpCert"
            "Test/Golden/Sophie/Node/KeyGen"
            "Test/Golden/Sophie/Node/KeyGenKes"
            "Test/Golden/Sophie/Node/KeyGenVrf"
            "Test/Golden/Sophie/StakeAddress/Build"
            "Test/Golden/Sophie/StakeAddress/DeregistrationCertificate"
            "Test/Golden/Sophie/StakeAddress/KeyGen"
            "Test/Golden/Sophie/StakeAddress/RegistrationCertificate"
            "Test/Golden/Sophie/StakePool/RegistrationCertificate"
            "Test/Golden/Sophie/TextEnvelope/Certificates/GenesisKeyDelegationCertificate"
            "Test/Golden/Sophie/TextEnvelope/Certificates/MIRCertificate"
            "Test/Golden/Sophie/TextEnvelope/Certificates/OperationalCertificate"
            "Test/Golden/Sophie/TextEnvelope/Certificates/StakeAddressCertificates"
            "Test/Golden/Sophie/TextEnvelope/Certificates/StakePoolCertificates"
            "Test/Golden/Sophie/TextEnvelope/Keys/ExtendedPaymentKeys"
            "Test/Golden/Sophie/TextEnvelope/Keys/GenesisDelegateKeys"
            "Test/Golden/Sophie/TextEnvelope/Keys/GenesisKeys"
            "Test/Golden/Sophie/TextEnvelope/Keys/GenesisUTxOKeys"
            "Test/Golden/Sophie/TextEnvelope/Keys/KESKeys"
            "Test/Golden/Sophie/TextEnvelope/Keys/PaymentKeys"
            "Test/Golden/Sophie/TextEnvelope/Keys/StakeKeys"
            "Test/Golden/Sophie/TextEnvelope/Keys/VRFKeys"
            "Test/Golden/Sophie/TextEnvelope/Tx/Tx"
            "Test/Golden/Sophie/TextEnvelope/Tx/TxBody"
            "Test/Golden/Sophie/TextEnvelope/Tx/Witness"
            "Test/Golden/Sophie/TextView/DecodeCbor"
            "Test/Golden/Sophie/Transaction/Assemble"
            "Test/Golden/Sophie/Transaction/Build"
            "Test/Golden/Sophie/Transaction/CalculateMinFee"
            "Test/Golden/Sophie/Transaction/CreateWitness"
            "Test/Golden/Sophie/Transaction/Sign"
            "Test/Golden/TxView"
            "Test/Golden/Version"
            "Test/OptParse"
            "Test/Utilities"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "bcc-cli-golden.hs" ];
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
    postUnpack = "sourceRoot+=/bcc-cli; echo source root reset to \$sourceRoot";
    }