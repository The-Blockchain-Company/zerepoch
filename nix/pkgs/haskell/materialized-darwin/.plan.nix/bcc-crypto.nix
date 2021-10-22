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
    flags = { golden-tests = false; golden-tests-exe = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "bcc-crypto"; version = "1.1.0"; };
      license = "MIT";
      copyright = "2016-2021 GodXCoin";
      maintainer = "contact@typed.io";
      author = "Vincent Hanquez";
      homepage = "https://github.com/The-Blockchain-Company/bcc-crypto#readme";
      url = "";
      synopsis = "Cryptography primitives for bcc";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" "cbits/*.h" "cbits/ed25519/*.h" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
          (hsPkgs."foundation" or (errorHandler.buildDepError "foundation"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
          ];
        buildable = true;
        modules = [
          "Bcc/Crypto/Wallet"
          "Bcc/Crypto/Wallet/Encrypted"
          "Bcc/Crypto/Wallet/Types"
          "Bcc/Crypto/Wallet/Pure"
          "Bcc/Crypto/Encoding/BIP39"
          "Bcc/Crypto/Encoding/Seed"
          "Crypto/Math/Edwards25519"
          "Crypto/Math/Bits"
          "Crypto/Math/Bytes"
          "Crypto/Math/NatMath"
          "Crypto/ECC/Ed25519Donna"
          "Crypto/ECC/Ed25519BIP32"
          "Crypto/Encoding/BIP39"
          "Crypto/Encoding/BIP39/Dictionary"
          "Crypto/Encoding/BIP39/English"
          "Bcc/Internal/Compat"
          ];
        cSources = [ "cbits/ed25519/ed25519.c" "cbits/encrypted_sign.c" ];
        hsSourceDirs = [ "src" ];
        includeDirs = [ "cbits/ed25519" "cbits" ];
        };
      exes = {
        "golden-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
            (hsPkgs."foundation" or (errorHandler.buildDepError "foundation"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."bcc-crypto" or (errorHandler.buildDepError "bcc-crypto"))
            ] ++ (pkgs.lib).optional (flags.golden-tests-exe) (hsPkgs."inspector" or (errorHandler.buildDepError "inspector"));
          buildable = if flags.golden-tests-exe then true else false;
          modules = [ "Test/Orphans" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "GoldenTest.hs" ] ++ [ "" ];
          };
        };
      tests = {
        "bcc-crypto-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."bcc-crypto" or (errorHandler.buildDepError "bcc-crypto"))
            (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
            (hsPkgs."foundation" or (errorHandler.buildDepError "foundation"))
            ];
          buildable = true;
          modules = [
            "Test/Crypto"
            "Test/Crypto/Encoding"
            "Test/Crypto/Encoding/BIP39"
            "Test/Bcc"
            "Test/Bcc/Crypto"
            "Test/Bcc/Crypto/Encoding"
            "Test/Bcc/Crypto/Encoding/Seed"
            "Utils"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        "bcc-crypto-golden-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
            (hsPkgs."foundation" or (errorHandler.buildDepError "foundation"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."bcc-crypto" or (errorHandler.buildDepError "bcc-crypto"))
            ] ++ (pkgs.lib).optional (flags.golden-tests) (hsPkgs."inspector" or (errorHandler.buildDepError "inspector"));
          buildable = if flags.golden-tests then true else false;
          modules = [ "Test/Orphans" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "GoldenTest.hs" ];
          };
        };
      benchmarks = {
        "bcc-crypto-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."bcc-crypto" or (errorHandler.buildDepError "bcc-crypto"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            ];
          buildable = true;
          hsSourceDirs = [ "benchs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "3";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "3";
      rev = "minimal";
      sha256 = "";
      };
    }