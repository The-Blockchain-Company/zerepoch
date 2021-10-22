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
    flags = { use-ghc-stub = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "zerepoch-tx-plugin"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@bcccoin.io";
      author = "Michael Peyton Jones";
      homepage = "";
      url = "";
      synopsis = "The Zerepoch Tx compiler and GHC plugin";
      description = "The Zerepoch Tx compiler and GHC plugin.";
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
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
          ] ++ (if flags.use-ghc-stub
          then [
            (hsPkgs."zerepoch-ghc-stub" or (errorHandler.buildDepError "zerepoch-ghc-stub"))
            ]
          else [ (hsPkgs."ghc" or (errorHandler.buildDepError "ghc")) ]);
        buildable = true;
        modules = [
          "ZerepochTx/Compiler/Binders"
          "ZerepochTx/Compiler/Builtins"
          "ZerepochTx/Compiler/Expr"
          "ZerepochTx/Compiler/Kind"
          "ZerepochTx/Compiler/Laziness"
          "ZerepochTx/Compiler/Names"
          "ZerepochTx/Compiler/Type"
          "ZerepochTx/Compiler/Types"
          "ZerepochTx/Compiler/Utils"
          "ZerepochTx/PIRTypes"
          "ZerepochTx/PLCTypes"
          "ZerepochTx/Plugin"
          "ZerepochTx/Compiler/Error"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "zerepoch-tx-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
            (hsPkgs."zerepoch-tx-plugin" or (errorHandler.buildDepError "zerepoch-tx-plugin"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            ];
          buildable = if flags.use-ghc-stub then false else true;
          modules = [
            "IsData/Spec"
            "Lift/Spec"
            "Plugin/Spec"
            "Plugin/Basic/Spec"
            "Plugin/Data/Spec"
            "Plugin/Errors/Spec"
            "Plugin/Functions/Spec"
            "Plugin/Laziness/Spec"
            "Plugin/Primitives/Spec"
            "Plugin/Typeclasses/Spec"
            "Plugin/Typeclasses/Lib"
            "Plugin/Lib"
            "StdLib/Spec"
            "TH/Spec"
            "TH/TestTH"
            "Lib"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../zerepoch-tx-plugin; }