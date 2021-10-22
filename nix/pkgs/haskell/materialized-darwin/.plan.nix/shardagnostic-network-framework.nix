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
      specVersion = "1.10";
      identifier = {
        name = "shardagnostic-network-framework";
        version = "0.1.0.0";
        };
      license = "Apache-2.0";
      copyright = "2021 The-Blockchain-Company ";
      maintainer = "alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@bcccoin.io";
      author = "Alexander Vieth, Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "CHANGELOG.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
          (hsPkgs."typed-protocols-examples" or (errorHandler.buildDepError "typed-protocols-examples"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
        buildable = true;
        modules = [
          "Shardagnostic/Network/Linger"
          "Shardagnostic/Network/Codec"
          "Shardagnostic/Network/CodecCBORTerm"
          "Shardagnostic/Network/Channel"
          "Shardagnostic/Network/Driver"
          "Shardagnostic/Network/Driver/Simple"
          "Shardagnostic/Network/Driver/Limits"
          "Shardagnostic/Network/ErrorPolicy"
          "Shardagnostic/Network/IOManager"
          "Shardagnostic/Network/Mux"
          "Shardagnostic/Network/Util/ShowProxy"
          "Shardagnostic/Network/Protocol/Handshake"
          "Shardagnostic/Network/Protocol/Handshake/Type"
          "Shardagnostic/Network/Protocol/Handshake/Codec"
          "Shardagnostic/Network/Protocol/Handshake/Client"
          "Shardagnostic/Network/Protocol/Handshake/Server"
          "Shardagnostic/Network/Protocol/Handshake/Version"
          "Shardagnostic/Network/Protocol/Handshake/Unversioned"
          "Shardagnostic/Network/Protocol/Limits"
          "Shardagnostic/Network/ConnectionId"
          "Shardagnostic/Network/Server/ConnectionTable"
          "Shardagnostic/Network/Server/Socket"
          "Shardagnostic/Network/Server/RateLimiting"
          "Shardagnostic/Network/Snocket"
          "Shardagnostic/Network/Socket"
          "Shardagnostic/Network/Subscription"
          "Shardagnostic/Network/Subscription/Client"
          "Shardagnostic/Network/Subscription/Dns"
          "Shardagnostic/Network/Subscription/Ip"
          "Shardagnostic/Network/Subscription/PeerState"
          "Shardagnostic/Network/Subscription/Subscriber"
          "Shardagnostic/Network/Subscription/Worker"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "demo-ping-pong" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."shardagnostic-network-framework" or (errorHandler.buildDepError "shardagnostic-network-framework"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols-examples" or (errorHandler.buildDepError "typed-protocols-examples"))
            ] ++ (pkgs.lib).optionals (system.isWindows) [
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            ];
          buildable = true;
          modules = [ "Network/TypedProtocol/PingPong/Codec/CBOR" ];
          hsSourceDirs = [ "demo" "test" ];
          mainPath = [
            "ping-pong.hs"
            ] ++ (pkgs.lib).optional (system.isWindows) "";
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."shardagnostic-network-framework" or (errorHandler.buildDepError "shardagnostic-network-framework"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."typed-protocols-examples" or (errorHandler.buildDepError "typed-protocols-examples"))
            ] ++ (pkgs.lib).optionals (system.isWindows) [
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            ];
          buildable = true;
          modules = [
            "Network/TypedProtocol/PingPong/Codec/CBOR"
            "Network/TypedProtocol/ReqResp/Codec/CBOR"
            "Test/Network/TypedProtocol/PingPong/Codec"
            "Test/Network/TypedProtocol/ReqResp/Codec"
            "Test/Shardagnostic/Network/Driver"
            "Test/Shardagnostic/Network/Orphans"
            "Test/Shardagnostic/Network/Socket"
            "Test/Shardagnostic/Network/Subscription"
            "Test/Shardagnostic/Network/RateLimiting"
            ];
          hsSourceDirs = [ "test" ];
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
    postUnpack = "sourceRoot+=/shardagnostic-network-framework; echo source root reset to \$sourceRoot";
    }