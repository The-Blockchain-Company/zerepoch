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
    flags = { asserts = false; ipv6 = false; cddl = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "shardagnostic-network"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2021 The-Blockchain-Company ";
      maintainer = "";
      author = "Alexander Vieth, Marcin Szamotulski, Duncan Coutts";
      homepage = "";
      url = "";
      synopsis = "A networking layer for the Shardagnostic blockchain protocol";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [
        "test-cddl/specs/handshake-node-to-node.cddl"
        "test-cddl/specs/handshake-node-to-client.cddl"
        "test-cddl/specs/chain-sync.cddl"
        "test-cddl/specs/block-fetch.cddl"
        "test-cddl/specs/tx-submission.cddl"
        "test-cddl/specs/tx-submission2.cddl"
        "test-cddl/specs/keep-alive.cddl"
        "test-cddl/specs/local-tx-submission.cddl"
        "test-cddl/specs/local-state-query.cddl"
        ];
      extraSrcFiles = [ "ChangeLog.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
          (hsPkgs."fingertree" or (errorHandler.buildDepError "fingertree"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."bcc-slotting" or (errorHandler.buildDepError "bcc-slotting"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."shardagnostic-network-framework" or (errorHandler.buildDepError "shardagnostic-network-framework"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
        buildable = true;
        modules = [
          "Shardagnostic/Network/PeerSelection/Governor/ActivePeers"
          "Shardagnostic/Network/PeerSelection/Governor/EstablishedPeers"
          "Shardagnostic/Network/PeerSelection/Governor/KnownPeers"
          "Shardagnostic/Network/PeerSelection/Governor/Monitor"
          "Shardagnostic/Network/PeerSelection/Governor/RootPeers"
          "Shardagnostic/Network/PeerSelection/Governor/Types"
          "Shardagnostic/Network/AnchoredFragment"
          "Shardagnostic/Network/AnchoredSeq"
          "Shardagnostic/Network/Block"
          "Shardagnostic/Network/BlockFetch"
          "Shardagnostic/Network/BlockFetch/Client"
          "Shardagnostic/Network/BlockFetch/ClientRegistry"
          "Shardagnostic/Network/BlockFetch/ClientState"
          "Shardagnostic/Network/BlockFetch/Decision"
          "Shardagnostic/Network/BlockFetch/DeltaQ"
          "Shardagnostic/Network/BlockFetch/State"
          "Shardagnostic/Network/Counter"
          "Shardagnostic/Network/DeltaQ"
          "Shardagnostic/Network/Diffusion"
          "Shardagnostic/Network/KeepAlive"
          "Shardagnostic/Network/Magic"
          "Shardagnostic/Network/NodeToNode"
          "Shardagnostic/Network/NodeToNode/Version"
          "Shardagnostic/Network/NodeToClient"
          "Shardagnostic/Network/NodeToClient/Version"
          "Shardagnostic/Network/Tracers"
          "Shardagnostic/Network/Point"
          "Shardagnostic/Network/PeerSelection/Types"
          "Shardagnostic/Network/PeerSelection/EstablishedPeers"
          "Shardagnostic/Network/PeerSelection/KnownPeers"
          "Shardagnostic/Network/PeerSelection/LedgerPeers"
          "Shardagnostic/Network/PeerSelection/LocalRootPeers"
          "Shardagnostic/Network/PeerSelection/RootPeersDNS"
          "Shardagnostic/Network/PeerSelection/Governor"
          "Shardagnostic/Network/Protocol/ChainSync/Client"
          "Shardagnostic/Network/Protocol/ChainSync/ClientPipelined"
          "Shardagnostic/Network/Protocol/ChainSync/Codec"
          "Shardagnostic/Network/Protocol/ChainSync/Server"
          "Shardagnostic/Network/Protocol/ChainSync/Type"
          "Shardagnostic/Network/Protocol/ChainSync/PipelineDecision"
          "Shardagnostic/Network/Protocol/ChainSync/Examples"
          "Shardagnostic/Network/Protocol/BlockFetch/Type"
          "Shardagnostic/Network/Protocol/BlockFetch/Client"
          "Shardagnostic/Network/Protocol/BlockFetch/Server"
          "Shardagnostic/Network/Protocol/BlockFetch/Codec"
          "Shardagnostic/Network/Protocol/LocalStateQuery/Client"
          "Shardagnostic/Network/Protocol/LocalStateQuery/Codec"
          "Shardagnostic/Network/Protocol/LocalStateQuery/Examples"
          "Shardagnostic/Network/Protocol/LocalStateQuery/Server"
          "Shardagnostic/Network/Protocol/LocalStateQuery/Type"
          "Shardagnostic/Network/Protocol/LocalTxMonitor/Type"
          "Shardagnostic/Network/Protocol/TipSample/Type"
          "Shardagnostic/Network/Protocol/TipSample/Client"
          "Shardagnostic/Network/Protocol/TipSample/Server"
          "Shardagnostic/Network/Protocol/TipSample/Codec"
          "Shardagnostic/Network/Protocol/TxSubmission/Type"
          "Shardagnostic/Network/Protocol/TxSubmission/Client"
          "Shardagnostic/Network/Protocol/TxSubmission/Server"
          "Shardagnostic/Network/Protocol/TxSubmission/Codec"
          "Shardagnostic/Network/Protocol/TxSubmission2/Type"
          "Shardagnostic/Network/Protocol/TxSubmission2/Codec"
          "Shardagnostic/Network/Protocol/LocalTxSubmission/Type"
          "Shardagnostic/Network/Protocol/LocalTxSubmission/Client"
          "Shardagnostic/Network/Protocol/LocalTxSubmission/Server"
          "Shardagnostic/Network/Protocol/LocalTxSubmission/Codec"
          "Shardagnostic/Network/Protocol/KeepAlive/Type"
          "Shardagnostic/Network/Protocol/KeepAlive/Client"
          "Shardagnostic/Network/Protocol/KeepAlive/Server"
          "Shardagnostic/Network/Protocol/KeepAlive/Codec"
          "Shardagnostic/Network/Protocol/Trans/Hello/Type"
          "Shardagnostic/Network/Protocol/Trans/Hello/Codec"
          "Shardagnostic/Network/Protocol/Trans/Hello/Util"
          "Shardagnostic/Network/TxSubmission/Inbound"
          "Shardagnostic/Network/TxSubmission/Mempool/Reader"
          "Shardagnostic/Network/TxSubmission/Outbound"
          "Shardagnostic/Network/MockChain/Chain"
          "Shardagnostic/Network/MockChain/ProducerState"
          "Shardagnostic/Network/Testing/ConcreteBlock"
          ];
        hsSourceDirs = [ "src" ];
        };
      sublibs = {
        "shardagnostic-protocol-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."pipes" or (errorHandler.buildDepError "pipes"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."bcc-slotting" or (errorHandler.buildDepError "bcc-slotting"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."shardagnostic-network" or (errorHandler.buildDepError "shardagnostic-network"))
            (hsPkgs."shardagnostic-network-framework" or (errorHandler.buildDepError "shardagnostic-network-framework"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            ];
          buildable = true;
          modules = [
            "Shardagnostic/Network/Protocol/BlockFetch/Direct"
            "Shardagnostic/Network/Protocol/BlockFetch/Examples"
            "Shardagnostic/Network/Protocol/BlockFetch/Test"
            "Shardagnostic/Network/Protocol/ChainSync/Direct"
            "Shardagnostic/Network/Protocol/ChainSync/DirectPipelined"
            "Shardagnostic/Network/Protocol/ChainSync/ExamplesPipelined"
            "Shardagnostic/Network/Protocol/ChainSync/Test"
            "Shardagnostic/Network/Protocol/Handshake/Direct"
            "Shardagnostic/Network/Protocol/Handshake/Test"
            "Shardagnostic/Network/Protocol/LocalStateQuery/Direct"
            "Shardagnostic/Network/Protocol/LocalStateQuery/Test"
            "Shardagnostic/Network/Protocol/LocalTxSubmission/Direct"
            "Shardagnostic/Network/Protocol/LocalTxSubmission/Examples"
            "Shardagnostic/Network/Protocol/LocalTxSubmission/Test"
            "Shardagnostic/Network/Protocol/TipSample/Direct"
            "Shardagnostic/Network/Protocol/TipSample/Examples"
            "Shardagnostic/Network/Protocol/TipSample/Test"
            "Shardagnostic/Network/Protocol/TxSubmission/Direct"
            "Shardagnostic/Network/Protocol/TxSubmission/Examples"
            "Shardagnostic/Network/Protocol/TxSubmission/Test"
            "Shardagnostic/Network/Protocol/TxSubmission2/Test"
            "Shardagnostic/Network/Protocol/KeepAlive/Direct"
            "Shardagnostic/Network/Protocol/KeepAlive/Examples"
            "Shardagnostic/Network/Protocol/KeepAlive/Test"
            "Test/ChainGenerators"
            "Test/ChainProducerState"
            "Test/Shardagnostic/Network/Testing/Utils"
            ];
          hsSourceDirs = [ "protocol-tests" ];
          };
        };
      exes = {
        "demo-chain-sync" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."shardagnostic-network-framework" or (errorHandler.buildDepError "shardagnostic-network-framework"))
            (hsPkgs."shardagnostic-network" or (errorHandler.buildDepError "shardagnostic-network"))
            ];
          buildable = true;
          hsSourceDirs = [ "demo" ];
          mainPath = [ "chain-sync.hs" ];
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
            (hsPkgs."bcc-slotting" or (errorHandler.buildDepError "bcc-slotting"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."shardagnostic-network" or (errorHandler.buildDepError "shardagnostic-network"))
            (hsPkgs."shardagnostic-network-framework" or (errorHandler.buildDepError "shardagnostic-network-framework"))
            (hsPkgs."shardagnostic-network-testing" or (errorHandler.buildDepError "shardagnostic-network-testing"))
            (hsPkgs."shardagnostic-network".components.sublibs.shardagnostic-protocol-tests or (errorHandler.buildDepError "shardagnostic-network:shardagnostic-protocol-tests"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            ] ++ (pkgs.lib).optionals (system.isWindows) [
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            ];
          buildable = true;
          modules = [
            "Shardagnostic/Network/BlockFetch/Examples"
            "Shardagnostic/Network/MockNode"
            "Test/AnchoredFragment"
            "Test/Chain"
            "Test/LedgerPeers"
            "Test/Shardagnostic/Network/BlockFetch"
            "Test/Shardagnostic/Network/KeepAlive"
            "Test/Shardagnostic/Network/MockNode"
            "Test/Shardagnostic/Network/TxSubmission"
            "Test/Shardagnostic/Network/PeerSelection"
            "Test/Shardagnostic/Network/PeerSelection/Instances"
            "Test/Shardagnostic/Network/PeerSelection/LocalRootPeers"
            "Test/Shardagnostic/Network/PeerSelection/MockEnvironment"
            "Test/Shardagnostic/Network/PeerSelection/PeerGraph"
            "Test/Shardagnostic/Network/PeerSelection/Script"
            "Test/Shardagnostic/Network/NodeToNode/Version"
            "Test/Shardagnostic/Network/NodeToClient/Version"
            "Test/Mux"
            "Test/Pipe"
            "Test/Socket"
            "Test/PeerState"
            "Test/Version"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        "cddl" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."process-extras" or (errorHandler.buildDepError "process-extras"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."shardagnostic-network-framework" or (errorHandler.buildDepError "shardagnostic-network-framework"))
            (hsPkgs."shardagnostic-network" or (errorHandler.buildDepError "shardagnostic-network"))
            (hsPkgs."shardagnostic-network".components.sublibs.shardagnostic-protocol-tests or (errorHandler.buildDepError "shardagnostic-network:shardagnostic-protocol-tests"))
            ];
          buildable = if flags.cddl then true else false;
          hsSourceDirs = [ "test-cddl" ];
          mainPath = [ "Main.hs" ];
          };
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
    postUnpack = "sourceRoot+=/shardagnostic-network; echo source root reset to \$sourceRoot";
    }