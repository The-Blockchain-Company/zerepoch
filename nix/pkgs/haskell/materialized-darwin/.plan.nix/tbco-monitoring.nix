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
    flags = { disable-observables = false; performance-test-queue = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "tbco-monitoring"; version = "0.2.0.0"; };
      license = "Apache-2.0";
      copyright = "2018 The Blockchain Co.";
      maintainer = "operations@blockchain-company.io";
      author = "Alexander Diemand, Andreas Triantafyllos";
      homepage = "";
      url = "";
      synopsis = "logging, benchmarking and monitoring framework";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "README.md"
        "src/Bcc/BM/Counters/os-support-darwin.h"
        "src/Bcc/BM/Counters/os-support-win.h"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."async-timer" or (errorHandler.buildDepError "async-timer"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."ekg" or (errorHandler.buildDepError "ekg"))
          (hsPkgs."katip" or (errorHandler.buildDepError "katip"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."time-units" or (errorHandler.buildDepError "time-units"))
          (hsPkgs."tracer-transformers" or (errorHandler.buildDepError "tracer-transformers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          (hsPkgs."libyaml" or (errorHandler.buildDepError "libyaml"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        buildable = true;
        modules = ((([
          "Paths_tbco_monitoring"
          "Bcc/BM/Configuration"
          "Bcc/BM/Configuration/Model"
          "Bcc/BM/Configuration/Static"
          "Bcc/BM/Counters"
          "Bcc/BM/Counters/Common"
          "Bcc/BM/Counters/Dummy"
          "Bcc/BM/Stats"
          "Bcc/BM/Stats/Resources"
          "Bcc/BM/Data/Aggregated"
          "Bcc/BM/Data/AggregatedKind"
          "Bcc/BM/Data/Backend"
          "Bcc/BM/Data/BackendKind"
          "Bcc/BM/Data/Configuration"
          "Bcc/BM/Data/Counter"
          "Bcc/BM/Data/LogItem"
          "Bcc/BM/Data/MonitoringEval"
          "Bcc/BM/Data/Observable"
          "Bcc/BM/Data/Output"
          "Bcc/BM/Data/Rotation"
          "Bcc/BM/Data/Severity"
          "Bcc/BM/Data/SubTrace"
          "Bcc/BM/Data/Trace"
          "Bcc/BM/Data/Tracer"
          "Bcc/BM/Data/Transformers"
          "Bcc/BM/Internal/ElidingTracer"
          "Bcc/BM/Tracing"
          "Bcc/BM/Backend/Log"
          "Bcc/BM/Backend/LogBuffer"
          "Bcc/BM/Backend/ProcessQueue"
          "Bcc/BM/Backend/Switchboard"
          "Bcc/BM/Plugin"
          "Bcc/BM/Rotator"
          "Bcc/BM/Setup"
          "Bcc/BM/Trace"
          "Bcc/BM/Tracer"
          "Bcc/BM/IOManager"
          "Bcc/BM/Snocket"
          ] ++ (pkgs.lib).optionals (!flags.disable-observables) [
          "Bcc/BM/Observer/Monadic"
          "Bcc/BM/Observer/STM"
          ]) ++ (pkgs.lib).optional (system.isLinux) "Bcc/BM/Counters/Linux") ++ (pkgs.lib).optional (system.isWindows) "Bcc/BM/Counters/Windows") ++ (pkgs.lib).optional (system.isOsx) "Bcc/BM/Counters/Darwin";
        cSources = (pkgs.lib).optional (system.isWindows) "src/Bcc/BM/Counters/os-support-win.c" ++ (pkgs.lib).optional (system.isOsx) "src/Bcc/BM/Counters/os-support-darwin.c";
        hsSourceDirs = [ "src" ];
        includeDirs = (pkgs.lib).optional (system.isWindows) "src/Bcc/BM/Counters/" ++ (pkgs.lib).optional (system.isOsx) "src/Bcc/BM/Counters/";
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."tbco-monitoring" or (errorHandler.buildDepError "tbco-monitoring"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-units" or (errorHandler.buildDepError "time-units"))
            (hsPkgs."tracer-transformers" or (errorHandler.buildDepError "tracer-transformers"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."void" or (errorHandler.buildDepError "void"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."libyaml" or (errorHandler.buildDepError "libyaml"))
            ];
          buildable = true;
          modules = [
            "Bcc/BM/Test/Trace"
            "Bcc/BM/Test/STM"
            "Bcc/BM/Test/Configuration"
            "Bcc/BM/Test/LogItem"
            "Bcc/BM/Test/Mock"
            "Bcc/BM/Test/Rotator"
            "Bcc/BM/Test/Routing"
            "Bcc/BM/Test/Structured"
            "Bcc/BM/Test/Tracer"
            "Bcc/BM/Test/Aggregated"
            "Bcc/BM/Arbitrary"
            "Bcc/BM/Arbitrary/Aggregated"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Test.lhs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "7";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "7";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/tbco-monitoring; echo source root reset to \$sourceRoot";
    }