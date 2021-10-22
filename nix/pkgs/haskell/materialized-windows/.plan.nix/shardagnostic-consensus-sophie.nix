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
        name = "shardagnostic-consensus-sophie";
        version = "0.1.0.0";
        };
      license = "Apache-2.0";
      copyright = "2021 The-Blockchain-Company ";
      maintainer = "operations@bcccoin.io";
      author = "GodXCoin Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Sophie ledger integration in the Shardagnostic consensus layer";
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
          (hsPkgs."base-deriving-via" or (errorHandler.buildDepError "base-deriving-via"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
          (hsPkgs."bcc-ledger-core" or (errorHandler.buildDepError "bcc-ledger-core"))
          (hsPkgs."bcc-slotting" or (errorHandler.buildDepError "bcc-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."measures" or (errorHandler.buildDepError "measures"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."orphans-deriving-via" or (errorHandler.buildDepError "orphans-deriving-via"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."bcc-ledger-aurum" or (errorHandler.buildDepError "bcc-ledger-aurum"))
          (hsPkgs."bcc-ledger-sophie-ma" or (errorHandler.buildDepError "bcc-ledger-sophie-ma"))
          (hsPkgs."sophie-spec-ledger" or (errorHandler.buildDepError "sophie-spec-ledger"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."shardagnostic-network" or (errorHandler.buildDepError "shardagnostic-network"))
          (hsPkgs."shardagnostic-consensus" or (errorHandler.buildDepError "shardagnostic-consensus"))
          ];
        buildable = true;
        modules = [
          "Shardagnostic/Consensus/Sophie/Eras"
          "Shardagnostic/Consensus/Sophie/Ledger"
          "Shardagnostic/Consensus/Sophie/Ledger/Block"
          "Shardagnostic/Consensus/Sophie/Ledger/Config"
          "Shardagnostic/Consensus/Sophie/Ledger/Forge"
          "Shardagnostic/Consensus/Sophie/Ledger/Inspect"
          "Shardagnostic/Consensus/Sophie/Ledger/Integrity"
          "Shardagnostic/Consensus/Sophie/Ledger/Ledger"
          "Shardagnostic/Consensus/Sophie/Ledger/Mempool"
          "Shardagnostic/Consensus/Sophie/Ledger/NetworkProtocolVersion"
          "Shardagnostic/Consensus/Sophie/Ledger/Query"
          "Shardagnostic/Consensus/Sophie/Ledger/PeerSelection"
          "Shardagnostic/Consensus/Sophie/Ledger/TPraos"
          "Shardagnostic/Consensus/Sophie/Node"
          "Shardagnostic/Consensus/Sophie/Node/Serialisation"
          "Shardagnostic/Consensus/Sophie/Protocol"
          "Shardagnostic/Consensus/Sophie/Protocol/HotKey"
          "Shardagnostic/Consensus/Sophie/Protocol/Util"
          "Shardagnostic/Consensus/Sophie/SophieBased"
          "Shardagnostic/Consensus/Sophie/SophieHFC"
          ];
        hsSourceDirs = [ "src" ];
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
    postUnpack = "sourceRoot+=/shardagnostic-consensus-sophie; echo source root reset to \$sourceRoot";
    }