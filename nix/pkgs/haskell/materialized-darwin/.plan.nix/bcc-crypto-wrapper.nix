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
      identifier = { name = "bcc-crypto-wrapper"; version = "1.3.0"; };
      license = "Apache-2.0";
      copyright = "2019 GodXCoin";
      maintainer = "operations@bcccoin.io";
      author = "GodXCoin";
      homepage = "";
      url = "";
      synopsis = "Cryptographic primitives used in the Bcc project";
      description = "Cryptographic primitives used in the Bcc project";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
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
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."base64-bytestring-type" or (errorHandler.buildDepError "base64-bytestring-type"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."canonical-json" or (errorHandler.buildDepError "canonical-json"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."bcc-crypto" or (errorHandler.buildDepError "bcc-crypto"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        modules = [
          "Bcc/Crypto/Signing/Tag"
          "Bcc/Crypto/Signing/KeyGen"
          "Bcc/Crypto/Signing/VerificationKey"
          "Bcc/Crypto/Signing/SigningKey"
          "Bcc/Crypto/Signing/Signature"
          "Bcc/Crypto/Signing/Redeem/Compact"
          "Bcc/Crypto/Signing/Redeem/KeyGen"
          "Bcc/Crypto/Signing/Redeem/SigningKey"
          "Bcc/Crypto/Signing/Redeem/Signature"
          "Bcc/Crypto/Signing/Redeem/VerificationKey"
          "Bcc/Crypto/Signing/Safe/KeyGen"
          "Bcc/Crypto/Signing/Safe/PassPhrase"
          "Bcc/Crypto/Signing/Safe/SafeSigner"
          "Bcc/Crypto"
          "Bcc/Crypto/Hashing"
          "Bcc/Crypto/Orphans"
          "Bcc/Crypto/ProtocolMagic"
          "Bcc/Crypto/Random"
          "Bcc/Crypto/Signing"
          "Bcc/Crypto/Signing/Redeem"
          "Bcc/Crypto/Signing/Safe"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
            (hsPkgs."bcc-binary-test" or (errorHandler.buildDepError "bcc-binary-test"))
            (hsPkgs."bcc-crypto" or (errorHandler.buildDepError "bcc-crypto"))
            (hsPkgs."bcc-crypto-wrapper" or (errorHandler.buildDepError "bcc-crypto-wrapper"))
            (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
            (hsPkgs."bcc-prelude-test" or (errorHandler.buildDepError "bcc-prelude-test"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [
            "Test/Bcc/Crypto/CBOR"
            "Test/Bcc/Crypto/Dummy"
            "Test/Bcc/Crypto/Example"
            "Test/Bcc/Crypto/Gen"
            "Test/Bcc/Crypto/Hashing"
            "Test/Bcc/Crypto/Json"
            "Test/Bcc/Crypto/Keys"
            "Test/Bcc/Crypto/Limits"
            "Test/Bcc/Crypto/Orphans"
            "Test/Bcc/Crypto/Random"
            "Test/Bcc/Crypto/Signing/Redeem"
            "Test/Bcc/Crypto/Signing/Redeem/Compact"
            "Test/Bcc/Crypto/Signing/Safe"
            "Test/Bcc/Crypto/Signing/Signing"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "test.hs" ];
          };
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
    postUnpack = "sourceRoot+=/cole/crypto; echo source root reset to \$sourceRoot";
    }