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
      identifier = { name = "zerepoch-errors"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "nikolaos.bezirgiannis@blockchain-company.io";
      author = "Nikolaos Bezirgiannis";
      homepage = "";
      url = "";
      synopsis = "The error codes of the Zerepoch compiler & runtime";
      description = "Contains the documentation and helper code of all the errors and their error-codes\nwhich can be thrown by the Zerepoch framework: compiler, interpreter, and client code";
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
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
          (hsPkgs."zerepoch-tx" or (errorHandler.buildDepError "zerepoch-tx"))
          (hsPkgs."zerepoch-tx-plugin" or (errorHandler.buildDepError "zerepoch-tx-plugin"))
          ];
        buildable = true;
        modules = [
          "Errors/TH/GenDocs"
          "Errors"
          "Errors/Docs"
          "Errors/TH/GenCodes"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "zerepoch-errors-next" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."zerepoch-errors" or (errorHandler.buildDepError "zerepoch-errors"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            ];
          buildable = true;
          hsSourceDirs = [ "exe-next" ];
          mainPath = [ "Main.hs" ];
          };
        "zerepoch-errors-bootstrap" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."zerepoch-errors" or (errorHandler.buildDepError "zerepoch-errors"))
            ];
          buildable = true;
          modules = [ "Errors/TH/Bootstrap" ];
          hsSourceDirs = [ "exe-bootstrap" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../zerepoch-errors; }