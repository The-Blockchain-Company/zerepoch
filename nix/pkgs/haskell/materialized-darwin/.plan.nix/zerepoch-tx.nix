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
      identifier = { name = "zerepoch-tx"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@blockchain-company.io";
      author = "Michael Peyton Jones";
      homepage = "";
      url = "";
      synopsis = "Libraries for Zerepoch Tx and its prelude";
      description = "Libraries for Zerepoch Tx and its prelude";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [ "README.md" ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          ];
        buildable = true;
        modules = [
          "ZerepochTx/IsData/Instances"
          "ZerepochTx/IsData/TH"
          "ZerepochTx/Lift/THUtils"
          "ZerepochTx/Lift/Instances"
          "ZerepochTx"
          "ZerepochTx/TH"
          "ZerepochTx/Prelude"
          "ZerepochTx/Evaluation"
          "ZerepochTx/Applicative"
          "ZerepochTx/Bool"
          "ZerepochTx/IsData"
          "ZerepochTx/IsData/Class"
          "ZerepochTx/Eq"
          "ZerepochTx/Enum"
          "ZerepochTx/Either"
          "ZerepochTx/Foldable"
          "ZerepochTx/Functor"
          "ZerepochTx/Lattice"
          "ZerepochTx/List"
          "ZerepochTx/Ord"
          "ZerepochTx/Maybe"
          "ZerepochTx/Monoid"
          "ZerepochTx/Numeric"
          "ZerepochTx/Ratio"
          "ZerepochTx/Semigroup"
          "ZerepochTx/Sqrt"
          "ZerepochTx/Traversable"
          "ZerepochTx/AssocMap"
          "ZerepochTx/Trace"
          "ZerepochTx/These"
          "ZerepochTx/Code"
          "ZerepochTx/Lift"
          "ZerepochTx/Lift/Class"
          "ZerepochTx/Builtins"
          "ZerepochTx/Builtins/Class"
          "ZerepochTx/Builtins/Internal"
          "ZerepochTx/Plugin/Utils"
          "ZerepochTx/Utils"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "zerepoch-tx-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.doctest.components.exes.doctest or (pkgs.buildPackages.doctest or (errorHandler.buildToolDepError "doctest:doctest")))
            ];
          buildable = if compiler.isGhcjs && true || system.isWindows
            then false
            else true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../zerepoch-tx; }