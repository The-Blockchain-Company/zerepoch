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
      specVersion = "2.2";
      identifier = { name = "zerepoch-benchmark"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "radu.ometita@bcccoin.io";
      author = "Radu Ometita";
      homepage = "https://github.com/bcccoin/zerepoch#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/The-Blockchain-Company/zerepoch#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [ "templates/*.tpl" "validation/data/*.flat" ];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
          (hsPkgs."zerepoch-tx-plugin" or (errorHandler.buildDepError "zerepoch-tx-plugin"))
          (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        modules = [
          "Zerepoch/Benchmark/Clausify"
          "Zerepoch/Benchmark/Queens"
          "Zerepoch/Benchmark/Knights"
          "Zerepoch/Benchmark/Knights/ChessSetList"
          "Zerepoch/Benchmark/Knights/KnightHeuristic"
          "Zerepoch/Benchmark/Knights/Queue"
          "Zerepoch/Benchmark/Knights/Sort"
          "Zerepoch/Benchmark/Knights/Utils"
          "Zerepoch/Benchmark/LastPiece"
          "Zerepoch/Benchmark/Prime"
          ];
        hsSourceDirs = [ "nofib/src" ];
        };
      exes = {
        "nofib-exe" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."zerepoch-benchmark" or (errorHandler.buildDepError "zerepoch-benchmark"))
            (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
            (hsPkgs."zerepoch-tx-plugin" or (errorHandler.buildDepError "zerepoch-tx-plugin"))
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          hsSourceDirs = [ "nofib/exe" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "zerepoch-benchmark-nofib-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."zerepoch-benchmark" or (errorHandler.buildDepError "zerepoch-benchmark"))
            (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
            (hsPkgs."zerepoch-tx-plugin" or (errorHandler.buildDepError "zerepoch-tx-plugin"))
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            ];
          buildable = true;
          hsSourceDirs = [ "nofib/test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      benchmarks = {
        "nofib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."zerepoch-benchmark" or (errorHandler.buildDepError "zerepoch-benchmark"))
            (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
            (hsPkgs."zerepoch-tx-plugin" or (errorHandler.buildDepError "zerepoch-tx-plugin"))
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            ];
          buildable = true;
          modules = [ "Common" "Paths_zerepoch_benchmark" ];
          hsSourceDirs = [ "nofib/bench" ];
          };
        "nofib-hs" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."zerepoch-benchmark" or (errorHandler.buildDepError "zerepoch-benchmark"))
            (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
            (hsPkgs."zerepoch-tx-plugin" or (errorHandler.buildDepError "zerepoch-tx-plugin"))
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = true;
          modules = [ "Common" "Paths_zerepoch_benchmark" ];
          hsSourceDirs = [ "nofib/bench" ];
          };
        "validation" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          modules = [ "NaturalSort" "Paths_zerepoch_benchmark" ];
          hsSourceDirs = [ "validation" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../zerepoch-benchmark; }