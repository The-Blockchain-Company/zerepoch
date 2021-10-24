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
      identifier = { name = "shardagnostic-consensus"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2021 The-Blockchain-Company ";
      maintainer = "operations@bcccoin.io";
      author = "The Blockchain Co. Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Consensus layer for the Shardagnostic blockchain protocol";
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
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."bcc-slotting" or (errorHandler.buildDepError "bcc-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."digest" or (errorHandler.buildDepError "digest"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filelock" or (errorHandler.buildDepError "filelock"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."measures" or (errorHandler.buildDepError "measures"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."semialign" or (errorHandler.buildDepError "semialign"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."shardagnostic-network-framework" or (errorHandler.buildDepError "shardagnostic-network-framework"))
          (hsPkgs."shardagnostic-network" or (errorHandler.buildDepError "shardagnostic-network"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."unix-bytestring" or (errorHandler.buildDepError "unix-bytestring"))
            ]);
        buildable = true;
        modules = [
          "Shardagnostic/Consensus/Block"
          "Shardagnostic/Consensus/Block/Abstract"
          "Shardagnostic/Consensus/Block/EBB"
          "Shardagnostic/Consensus/Block/Forging"
          "Shardagnostic/Consensus/Block/NestedContent"
          "Shardagnostic/Consensus/Block/RealPoint"
          "Shardagnostic/Consensus/Block/SupportsMetrics"
          "Shardagnostic/Consensus/Block/SupportsProtocol"
          "Shardagnostic/Consensus/BlockchainTime"
          "Shardagnostic/Consensus/BlockchainTime/API"
          "Shardagnostic/Consensus/BlockchainTime/WallClock/Default"
          "Shardagnostic/Consensus/BlockchainTime/WallClock/HardFork"
          "Shardagnostic/Consensus/BlockchainTime/WallClock/Simple"
          "Shardagnostic/Consensus/BlockchainTime/WallClock/Types"
          "Shardagnostic/Consensus/BlockchainTime/WallClock/Util"
          "Shardagnostic/Consensus/Config"
          "Shardagnostic/Consensus/Config/SecurityParam"
          "Shardagnostic/Consensus/Config/SupportsNode"
          "Shardagnostic/Consensus/Forecast"
          "Shardagnostic/Consensus/Fragment/Diff"
          "Shardagnostic/Consensus/Fragment/InFuture"
          "Shardagnostic/Consensus/Fragment/Validated"
          "Shardagnostic/Consensus/Fragment/ValidatedDiff"
          "Shardagnostic/Consensus/HardFork/Abstract"
          "Shardagnostic/Consensus/HardFork/Combinator"
          "Shardagnostic/Consensus/HardFork/Combinator/Abstract"
          "Shardagnostic/Consensus/HardFork/Combinator/Abstract/CanHardFork"
          "Shardagnostic/Consensus/HardFork/Combinator/Abstract/NoHardForks"
          "Shardagnostic/Consensus/HardFork/Combinator/Abstract/SingleEraBlock"
          "Shardagnostic/Consensus/HardFork/Combinator/AcrossEras"
          "Shardagnostic/Consensus/HardFork/Combinator/Basics"
          "Shardagnostic/Consensus/HardFork/Combinator/Block"
          "Shardagnostic/Consensus/HardFork/Combinator/Compat"
          "Shardagnostic/Consensus/HardFork/Combinator/Condense"
          "Shardagnostic/Consensus/HardFork/Combinator/Degenerate"
          "Shardagnostic/Consensus/HardFork/Combinator/Embed/Binary"
          "Shardagnostic/Consensus/HardFork/Combinator/Embed/Nary"
          "Shardagnostic/Consensus/HardFork/Combinator/Embed/Unary"
          "Shardagnostic/Consensus/HardFork/Combinator/Forging"
          "Shardagnostic/Consensus/HardFork/Combinator/Info"
          "Shardagnostic/Consensus/HardFork/Combinator/InjectTxs"
          "Shardagnostic/Consensus/HardFork/Combinator/Ledger"
          "Shardagnostic/Consensus/HardFork/Combinator/Ledger/CommonProtocolParams"
          "Shardagnostic/Consensus/HardFork/Combinator/Ledger/PeerSelection"
          "Shardagnostic/Consensus/HardFork/Combinator/Ledger/Query"
          "Shardagnostic/Consensus/HardFork/Combinator/Mempool"
          "Shardagnostic/Consensus/HardFork/Combinator/Node"
          "Shardagnostic/Consensus/HardFork/Combinator/Node/InitStorage"
          "Shardagnostic/Consensus/HardFork/Combinator/Node/Metrics"
          "Shardagnostic/Consensus/HardFork/Combinator/PartialConfig"
          "Shardagnostic/Consensus/HardFork/Combinator/Protocol"
          "Shardagnostic/Consensus/HardFork/Combinator/Protocol/ChainSel"
          "Shardagnostic/Consensus/HardFork/Combinator/Protocol/LedgerView"
          "Shardagnostic/Consensus/HardFork/Combinator/Serialisation"
          "Shardagnostic/Consensus/HardFork/Combinator/Serialisation/Common"
          "Shardagnostic/Consensus/HardFork/Combinator/Serialisation/SerialiseDisk"
          "Shardagnostic/Consensus/HardFork/Combinator/Serialisation/SerialiseNodeToClient"
          "Shardagnostic/Consensus/HardFork/Combinator/Serialisation/SerialiseNodeToNode"
          "Shardagnostic/Consensus/HardFork/Combinator/State"
          "Shardagnostic/Consensus/HardFork/Combinator/State/Infra"
          "Shardagnostic/Consensus/HardFork/Combinator/State/Instances"
          "Shardagnostic/Consensus/HardFork/Combinator/State/Lift"
          "Shardagnostic/Consensus/HardFork/Combinator/State/Types"
          "Shardagnostic/Consensus/HardFork/Combinator/Translation"
          "Shardagnostic/Consensus/HardFork/Combinator/Util/DerivingVia"
          "Shardagnostic/Consensus/HardFork/Combinator/Util/Functors"
          "Shardagnostic/Consensus/HardFork/Combinator/Util/InPairs"
          "Shardagnostic/Consensus/HardFork/Combinator/Util/Match"
          "Shardagnostic/Consensus/HardFork/Combinator/Util/Tails"
          "Shardagnostic/Consensus/HardFork/Combinator/Util/Telescope"
          "Shardagnostic/Consensus/HardFork/History"
          "Shardagnostic/Consensus/HardFork/History/Caching"
          "Shardagnostic/Consensus/HardFork/History/EpochInfo"
          "Shardagnostic/Consensus/HardFork/History/EraParams"
          "Shardagnostic/Consensus/HardFork/History/Qry"
          "Shardagnostic/Consensus/HardFork/History/Summary"
          "Shardagnostic/Consensus/HardFork/History/Util"
          "Shardagnostic/Consensus/HardFork/Simple"
          "Shardagnostic/Consensus/HeaderStateHistory"
          "Shardagnostic/Consensus/HeaderValidation"
          "Shardagnostic/Consensus/Ledger/Abstract"
          "Shardagnostic/Consensus/Ledger/Basics"
          "Shardagnostic/Consensus/Ledger/CommonProtocolParams"
          "Shardagnostic/Consensus/Ledger/Dual"
          "Shardagnostic/Consensus/Ledger/Extended"
          "Shardagnostic/Consensus/Ledger/Inspect"
          "Shardagnostic/Consensus/Ledger/Query"
          "Shardagnostic/Consensus/Ledger/Query/Version"
          "Shardagnostic/Consensus/Ledger/SupportsMempool"
          "Shardagnostic/Consensus/Ledger/SupportsPeerSelection"
          "Shardagnostic/Consensus/Ledger/SupportsProtocol"
          "Shardagnostic/Consensus/Mempool"
          "Shardagnostic/Consensus/Mempool/API"
          "Shardagnostic/Consensus/Mempool/Impl"
          "Shardagnostic/Consensus/Mempool/Impl/Pure"
          "Shardagnostic/Consensus/Mempool/Impl/Types"
          "Shardagnostic/Consensus/Mempool/TxLimits"
          "Shardagnostic/Consensus/Mempool/TxSeq"
          "Shardagnostic/Consensus/MiniProtocol/BlockFetch/Server"
          "Shardagnostic/Consensus/MiniProtocol/ChainSync/Client"
          "Shardagnostic/Consensus/MiniProtocol/ChainSync/Server"
          "Shardagnostic/Consensus/MiniProtocol/LocalStateQuery/Server"
          "Shardagnostic/Consensus/MiniProtocol/LocalTxSubmission/Server"
          "Shardagnostic/Consensus/Network/NodeToClient"
          "Shardagnostic/Consensus/Network/NodeToNode"
          "Shardagnostic/Consensus/Node"
          "Shardagnostic/Consensus/Node/DbLock"
          "Shardagnostic/Consensus/Node/DbMarker"
          "Shardagnostic/Consensus/Node/ErrorPolicy"
          "Shardagnostic/Consensus/Node/Exit"
          "Shardagnostic/Consensus/NodeId"
          "Shardagnostic/Consensus/NodeKernel"
          "Shardagnostic/Consensus/Node/InitStorage"
          "Shardagnostic/Consensus/Node/NetworkProtocolVersion"
          "Shardagnostic/Consensus/Node/ProtocolInfo"
          "Shardagnostic/Consensus/Node/Recovery"
          "Shardagnostic/Consensus/Node/Run"
          "Shardagnostic/Consensus/Node/Serialisation"
          "Shardagnostic/Consensus/Node/Tracers"
          "Shardagnostic/Consensus/Protocol/Abstract"
          "Shardagnostic/Consensus/Protocol/BFT"
          "Shardagnostic/Consensus/Protocol/LeaderSchedule"
          "Shardagnostic/Consensus/Protocol/MockChainSel"
          "Shardagnostic/Consensus/Protocol/ModChainSel"
          "Shardagnostic/Consensus/Protocol/PBFT"
          "Shardagnostic/Consensus/Protocol/PBFT/Crypto"
          "Shardagnostic/Consensus/Protocol/PBFT/State"
          "Shardagnostic/Consensus/Protocol/Signed"
          "Shardagnostic/Consensus/Ticked"
          "Shardagnostic/Consensus/TypeFamilyWrappers"
          "Shardagnostic/Consensus/Util"
          "Shardagnostic/Consensus/Util/AnchoredFragment"
          "Shardagnostic/Consensus/Util/Args"
          "Shardagnostic/Consensus/Util/Assert"
          "Shardagnostic/Consensus/Util/CallStack"
          "Shardagnostic/Consensus/Util/CBOR"
          "Shardagnostic/Consensus/Util/Condense"
          "Shardagnostic/Consensus/Util/Counting"
          "Shardagnostic/Consensus/Util/DepPair"
          "Shardagnostic/Consensus/Util/EarlyExit"
          "Shardagnostic/Consensus/Util/FileLock"
          "Shardagnostic/Consensus/Util/HList"
          "Shardagnostic/Consensus/Util/IOLike"
          "Shardagnostic/Consensus/Util/MonadSTM/NormalForm"
          "Shardagnostic/Consensus/Util/MonadSTM/RAWLock"
          "Shardagnostic/Consensus/Util/MonadSTM/StrictMVar"
          "Shardagnostic/Consensus/Util/OptNP"
          "Shardagnostic/Consensus/Util/Orphans"
          "Shardagnostic/Consensus/Util/RedundantConstraints"
          "Shardagnostic/Consensus/Util/ResourceRegistry"
          "Shardagnostic/Consensus/Util/Singletons"
          "Shardagnostic/Consensus/Util/SOP"
          "Shardagnostic/Consensus/Util/STM"
          "Shardagnostic/Consensus/Util/Time"
          "Shardagnostic/Consensus/Util/TraceSize"
          "Shardagnostic/Consensus/Util/Versioned"
          "Shardagnostic/Consensus/Storage/ChainDB"
          "Shardagnostic/Consensus/Storage/ChainDB/API"
          "Shardagnostic/Consensus/Storage/ChainDB/Impl"
          "Shardagnostic/Consensus/Storage/ChainDB/Impl/Args"
          "Shardagnostic/Consensus/Storage/ChainDB/Impl/Background"
          "Shardagnostic/Consensus/Storage/ChainDB/Impl/BlockCache"
          "Shardagnostic/Consensus/Storage/ChainDB/Impl/ChainSel"
          "Shardagnostic/Consensus/Storage/ChainDB/Impl/Follower"
          "Shardagnostic/Consensus/Storage/ChainDB/Impl/Iterator"
          "Shardagnostic/Consensus/Storage/ChainDB/Impl/LgrDB"
          "Shardagnostic/Consensus/Storage/ChainDB/Impl/Paths"
          "Shardagnostic/Consensus/Storage/ChainDB/Impl/Query"
          "Shardagnostic/Consensus/Storage/ChainDB/Impl/Types"
          "Shardagnostic/Consensus/Storage/ChainDB/Init"
          "Shardagnostic/Consensus/Storage/Common"
          "Shardagnostic/Consensus/Storage/FS/API"
          "Shardagnostic/Consensus/Storage/FS/API/Types"
          "Shardagnostic/Consensus/Storage/FS/CRC"
          "Shardagnostic/Consensus/Storage/FS/Handle"
          "Shardagnostic/Consensus/Storage/FS/IO"
          "Shardagnostic/Consensus/Storage/IO"
          "Shardagnostic/Consensus/Storage/ImmutableDB"
          "Shardagnostic/Consensus/Storage/ImmutableDB/API"
          "Shardagnostic/Consensus/Storage/ImmutableDB/Chunks"
          "Shardagnostic/Consensus/Storage/ImmutableDB/Chunks/Internal"
          "Shardagnostic/Consensus/Storage/ImmutableDB/Chunks/Layout"
          "Shardagnostic/Consensus/Storage/ImmutableDB/Impl"
          "Shardagnostic/Consensus/Storage/ImmutableDB/Impl/Index"
          "Shardagnostic/Consensus/Storage/ImmutableDB/Impl/Index/Cache"
          "Shardagnostic/Consensus/Storage/ImmutableDB/Impl/Index/Primary"
          "Shardagnostic/Consensus/Storage/ImmutableDB/Impl/Index/Secondary"
          "Shardagnostic/Consensus/Storage/ImmutableDB/Impl/Iterator"
          "Shardagnostic/Consensus/Storage/ImmutableDB/Impl/Parser"
          "Shardagnostic/Consensus/Storage/ImmutableDB/Impl/State"
          "Shardagnostic/Consensus/Storage/ImmutableDB/Impl/Types"
          "Shardagnostic/Consensus/Storage/ImmutableDB/Impl/Util"
          "Shardagnostic/Consensus/Storage/ImmutableDB/Impl/Validation"
          "Shardagnostic/Consensus/Storage/LedgerDB/DiskPolicy"
          "Shardagnostic/Consensus/Storage/LedgerDB/InMemory"
          "Shardagnostic/Consensus/Storage/LedgerDB/OnDisk"
          "Shardagnostic/Consensus/Storage/Serialisation"
          "Shardagnostic/Consensus/Storage/VolatileDB"
          "Shardagnostic/Consensus/Storage/VolatileDB/API"
          "Shardagnostic/Consensus/Storage/VolatileDB/Impl"
          "Shardagnostic/Consensus/Storage/VolatileDB/Impl/FileInfo"
          "Shardagnostic/Consensus/Storage/VolatileDB/Impl/Index"
          "Shardagnostic/Consensus/Storage/VolatileDB/Impl/Parser"
          "Shardagnostic/Consensus/Storage/VolatileDB/Impl/State"
          "Shardagnostic/Consensus/Storage/VolatileDB/Impl/Types"
          "Shardagnostic/Consensus/Storage/VolatileDB/Impl/Util"
          "Data/SOP/Strict"
          ] ++ (pkgs.lib).optional (system.isWindows) "Shardagnostic/Consensus/Storage/Seek";
        hsSourceDirs = [ "src" ] ++ (if system.isWindows
          then [ "src-win32" ]
          else [ "src-unix" ]);
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
    postUnpack = "sourceRoot+=/shardagnostic-consensus; echo source root reset to \$sourceRoot";
    }