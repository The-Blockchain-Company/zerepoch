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
      identifier = { name = "bcc-crypto-tests"; version = "2.0.0"; };
      license = "Apache-2.0";
      copyright = "2020-2021 The Blockchain Co.";
      maintainer = "operations@blockchain-company.io";
      author = "The Blockchain Co.";
      homepage = "";
      url = "";
      synopsis = "Tests for bcc-crypto-class and -optimum";
      description = "Tests for bcc-crypto-class and -optimum";
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
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
          (hsPkgs."bcc-crypto-optimum" or (errorHandler.buildDepError "bcc-crypto-optimum"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
          ];
        buildable = true;
        modules = [
          "Test/Crypto/DSIGN"
          "Test/Crypto/Hash"
          "Test/Crypto/KES"
          "Test/Crypto/Util"
          "Test/Crypto/VRF"
          "Test/Crypto/Instances"
          "Bench/Crypto/VRF"
          "Bench/Crypto/KES"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "test-crypto" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
            (hsPkgs."bcc-crypto-tests" or (errorHandler.buildDepError "bcc-crypto-tests"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      benchmarks = {
        "bench-crypto" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
            (hsPkgs."bcc-crypto-tests" or (errorHandler.buildDepError "bcc-crypto-tests"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            ];
          buildable = true;
          hsSourceDirs = [ "bench" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "4";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "4";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/bcc-crypto-tests; echo source root reset to \$sourceRoot";
    }