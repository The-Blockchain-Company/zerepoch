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
      identifier = { name = "bcc-crypto-test"; version = "1.3.0"; };
      license = "Apache-2.0";
      copyright = "2018 The Blockchain Co.";
      maintainer = "operations@bcccoin.io";
      author = "The Blockchain Co.";
      homepage = "";
      url = "";
      synopsis = "Test helpers from bcc-crypto exposed to other packages";
      description = "Test helpers from bcc-crypto exposed to other packages";
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
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."bcc-binary-test" or (errorHandler.buildDepError "bcc-binary-test"))
          (hsPkgs."bcc-crypto" or (errorHandler.buildDepError "bcc-crypto"))
          (hsPkgs."bcc-crypto-wrapper" or (errorHandler.buildDepError "bcc-crypto-wrapper"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."bcc-prelude-test" or (errorHandler.buildDepError "bcc-prelude-test"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          ];
        buildable = true;
        modules = [
          "Test/Bcc/Crypto/CBOR"
          "Test/Bcc/Crypto/Dummy"
          "Test/Bcc/Crypto/Example"
          "Test/Bcc/Crypto/Gen"
          "Test/Bcc/Crypto/Json"
          "Test/Bcc/Crypto/Orphans"
          ];
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
    postUnpack = "sourceRoot+=/cole/crypto/test; echo source root reset to \$sourceRoot";
    }