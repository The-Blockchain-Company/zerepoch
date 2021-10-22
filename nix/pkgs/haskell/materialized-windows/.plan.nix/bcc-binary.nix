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
      identifier = { name = "bcc-binary"; version = "1.5.0"; };
      license = "Apache-2.0";
      copyright = "2019-2021 GodXCoin";
      maintainer = "operations@bcccoin.io";
      author = "GodXCoin";
      homepage = "";
      url = "";
      synopsis = "Binary serialization for Bcc";
      description = "This package includes the binary serialization format for Bcc";
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
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-fix" or (errorHandler.buildDepError "data-fix"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."recursion-schemes" or (errorHandler.buildDepError "recursion-schemes"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        modules = [
          "Bcc/Binary/Annotated"
          "Bcc/Binary/Drop"
          "Bcc/Binary/Raw"
          "Bcc/Binary/ToCBOR"
          "Bcc/Binary/FromCBOR"
          "Bcc/Binary/Serialize"
          "Bcc/Binary/Deserialize"
          "Bcc/Binary"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
            (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
            (hsPkgs."bcc-prelude-test" or (errorHandler.buildDepError "bcc-prelude-test"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          modules = [
            "Test/Bcc/Binary/SizeBounds"
            "Test/Bcc/Binary/Helpers"
            "Test/Bcc/Binary/Helpers/GoldenRoundTrip"
            "Test/Bcc/Binary/RoundTrip"
            "Test/Bcc/Binary/Serialization"
            "Test/Bcc/Binary/Drop"
            "Test/Bcc/Binary/Failure"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "test.hs" ];
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
    postUnpack = "sourceRoot+=/binary; echo source root reset to \$sourceRoot";
    }