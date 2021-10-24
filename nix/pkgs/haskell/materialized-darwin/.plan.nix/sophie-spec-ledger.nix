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
      identifier = { name = "sophie-spec-ledger"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@bcccoin.io";
      author = "The Blockchain Co. Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Sophie Ledger Executable Model";
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
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."bcc-crypto" or (errorHandler.buildDepError "bcc-crypto"))
          (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
          (hsPkgs."bcc-crypto-wrapper" or (errorHandler.buildDepError "bcc-crypto-wrapper"))
          (hsPkgs."bcc-ledger-cole" or (errorHandler.buildDepError "bcc-ledger-cole"))
          (hsPkgs."bcc-ledger-core" or (errorHandler.buildDepError "bcc-ledger-core"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."bcc-slotting" or (errorHandler.buildDepError "bcc-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."sophie-spec-non-integral" or (errorHandler.buildDepError "sophie-spec-non-integral"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        modules = [
          "Sophie/Spec/Ledger/API/Types"
          "Bcc/Ledger/Pretty"
          "Bcc/Ledger/Sophie"
          "Bcc/Ledger/Sophie/Constraints"
          "Sophie/Spec/Ledger/Address"
          "Sophie/Spec/Ledger/Address/Bootstrap"
          "Sophie/Spec/Ledger/API"
          "Sophie/Spec/Ledger/API/ColeTranslation"
          "Sophie/Spec/Ledger/API/Genesis"
          "Sophie/Spec/Ledger/API/Protocol"
          "Sophie/Spec/Ledger/API/Validation"
          "Sophie/Spec/Ledger/API/Wallet"
          "Sophie/Spec/Ledger/API/Mempool"
          "Sophie/Spec/Ledger/BaseTypes"
          "Sophie/Spec/Ledger/BlockChain"
          "Sophie/Spec/Ledger/CompactAddr"
          "Sophie/Spec/Ledger/Credential"
          "Sophie/Spec/Ledger/Delegation/Certificates"
          "Sophie/Spec/Ledger/Delegation/PoolParams"
          "Sophie/Spec/Ledger/EpochBoundary"
          "Sophie/Spec/Ledger/Genesis"
          "Sophie/Spec/Ledger/HardForks"
          "Sophie/Spec/Ledger/Keys"
          "Sophie/Spec/Ledger/LedgerState"
          "Sophie/Spec/Ledger/Metadata"
          "Sophie/Spec/Ledger/OCert"
          "Sophie/Spec/Ledger/Orphans"
          "Sophie/Spec/Ledger/OverlaySchedule"
          "Sophie/Spec/Ledger/PParams"
          "Sophie/Spec/Ledger/Rewards"
          "Sophie/Spec/Ledger/RewardProvenance"
          "Sophie/Spec/Ledger/RewardUpdate"
          "Sophie/Spec/Ledger/Scripts"
          "Sophie/Spec/Ledger/Slot"
          "Sophie/Spec/Ledger/SoftForks"
          "Sophie/Spec/Ledger/StabilityWindow"
          "Sophie/Spec/Ledger/STS/Bbody"
          "Sophie/Spec/Ledger/STS/Chain"
          "Sophie/Spec/Ledger/STS/Deleg"
          "Sophie/Spec/Ledger/STS/Delegs"
          "Sophie/Spec/Ledger/STS/Delpl"
          "Sophie/Spec/Ledger/STS/Epoch"
          "Sophie/Spec/Ledger/STS/EraMapping"
          "Sophie/Spec/Ledger/STS/Ledger"
          "Sophie/Spec/Ledger/STS/Ledgers"
          "Sophie/Spec/Ledger/STS/Mir"
          "Sophie/Spec/Ledger/STS/NewEpoch"
          "Sophie/Spec/Ledger/STS/Newpp"
          "Sophie/Spec/Ledger/STS/Ocert"
          "Sophie/Spec/Ledger/STS/Overlay"
          "Sophie/Spec/Ledger/STS/Pool"
          "Sophie/Spec/Ledger/STS/PoolReap"
          "Sophie/Spec/Ledger/STS/Ppup"
          "Sophie/Spec/Ledger/STS/Prtcl"
          "Sophie/Spec/Ledger/STS/Rupd"
          "Sophie/Spec/Ledger/STS/Snap"
          "Sophie/Spec/Ledger/STS/Tick"
          "Sophie/Spec/Ledger/STS/Tickn"
          "Sophie/Spec/Ledger/STS/Updn"
          "Sophie/Spec/Ledger/STS/Upec"
          "Sophie/Spec/Ledger/STS/Utxo"
          "Sophie/Spec/Ledger/STS/Utxow"
          "Sophie/Spec/Ledger/Tx"
          "Sophie/Spec/Ledger/TxBody"
          "Sophie/Spec/Ledger/UTxO"
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
    postUnpack = "sourceRoot+=/sophie/chain-and-ledger/executable-spec; echo source root reset to \$sourceRoot";
    }