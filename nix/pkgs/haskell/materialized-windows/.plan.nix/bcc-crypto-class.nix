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
      identifier = { name = "bcc-crypto-class"; version = "2.0.0"; };
      license = "Apache-2.0";
      copyright = "2019-2021 The Blockchain Co.";
      maintainer = "operations@blockchain-company.io";
      author = "The Blockchain Co.";
      homepage = "";
      url = "";
      synopsis = "Type classes abstracting over cryptography primitives for Bcc";
      description = "Type classes abstracting over cryptography primitives for Bcc";
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
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        pkgconfig = [
          (pkgconfPkgs."libsodium" or (errorHandler.pkgConfDepError "libsodium"))
          ];
        buildable = true;
        modules = [
          "Bcc/Crypto/DSIGN"
          "Bcc/Crypto/Hash"
          "Bcc/Crypto/KES"
          "Bcc/Crypto/VRF"
          "Bcc/Crypto/DSIGN/Class"
          "Bcc/Crypto/DSIGN/Ed25519"
          "Bcc/Crypto/DSIGN/Ed448"
          "Bcc/Crypto/DSIGN/Mock"
          "Bcc/Crypto/DSIGN/NeverUsed"
          "Bcc/Crypto/Hash/Blake2b"
          "Bcc/Crypto/Hash/Class"
          "Bcc/Crypto/Hash/NeverUsed"
          "Bcc/Crypto/Hash/SHA256"
          "Bcc/Crypto/Hash/SHA3_256"
          "Bcc/Crypto/Hash/Short"
          "Bcc/Crypto/Hash/Keccak256"
          "Bcc/Crypto/KES/Class"
          "Bcc/Crypto/KES/Mock"
          "Bcc/Crypto/KES/NeverUsed"
          "Bcc/Crypto/KES/Simple"
          "Bcc/Crypto/KES/Single"
          "Bcc/Crypto/KES/Sum"
          "Bcc/Crypto/PinnedSizedBytes"
          "Bcc/Crypto/Seed"
          "Bcc/Crypto/Util"
          "Bcc/Crypto/VRF/Class"
          "Bcc/Crypto/VRF/Mock"
          "Bcc/Crypto/VRF/NeverUsed"
          "Bcc/Crypto/VRF/Simple"
          "Bcc/Crypto/Libsodium"
          "Bcc/Crypto/Libsodium/C"
          "Bcc/Crypto/Libsodium/Constants"
          "Bcc/Crypto/Libsodium/Hash"
          "Bcc/Crypto/Libsodium/Init"
          "Bcc/Crypto/Libsodium/Memory"
          "Bcc/Crypto/Libsodium/Memory/Internal"
          "Bcc/Crypto/Libsodium/MLockedBytes"
          "Bcc/Crypto/Libsodium/MLockedBytes/Internal"
          "Bcc/Crypto/Libsodium/UnsafeC"
          "Bcc/Foreign"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "test-memory-example" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
            ] ++ (pkgs.lib).optional (system.isLinux || system.isOsx) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = true;
          hsSourceDirs = [ "memory-example" ];
          mainPath = [ "Main.hs" ];
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
    postUnpack = "sourceRoot+=/bcc-crypto-class; echo source root reset to \$sourceRoot";
    }