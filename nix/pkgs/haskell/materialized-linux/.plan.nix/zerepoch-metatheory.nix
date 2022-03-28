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
      specVersion = "2.4";
      identifier = { name = "zerepoch-metatheory"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "james.chapman@blockchain-company.io";
      author = "James Chapman";
      homepage = "https://github.com/The-Blockchain-Company/zerepoch";
      url = "";
      synopsis = "Command line tool for running zerepoch core programs";
      description = "";
      buildType = "Custom";
      isLocal = true;
      setup-depends = [
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.process or (pkgs.buildPackages.process or (errorHandler.setupDepError "process")))
        (hsPkgs.buildPackages.turtle or (pkgs.buildPackages.turtle or (errorHandler.setupDepError "turtle")))
        ];
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "README.md"
        "Zerepoch.agda-lib"
        "src/**/*.lagda"
        "src/**/*.lagda.md"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."ieee754" or (errorHandler.buildDepError "ieee754"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        modules = [
          "MAurum/Code/Main"
          "MAurum/Code/Agda/Builtin/Bool"
          "MAurum/Code/Agda/Builtin/Char"
          "MAurum/Code/Agda/Builtin/Equality"
          "MAurum/Code/Agda/Builtin/IO"
          "MAurum/Code/Agda/Builtin/Int"
          "MAurum/Code/Agda/Builtin/List"
          "MAurum/Code/Agda/Builtin/Nat"
          "MAurum/Code/Agda/Builtin/Sigma"
          "MAurum/Code/Agda/Builtin/String"
          "MAurum/Code/Agda/Builtin/Unit"
          "MAurum/Code/Agda/Primitive"
          "MAurum/Code/Algebra/Bundles"
          "MAurum/Code/Algebra/Consequences/Base"
          "MAurum/Code/Algebra/Consequences/Setoid"
          "MAurum/Code/Algebra/Construct/LiftedChoice"
          "MAurum/Code/Algebra/Construct/NaturalChoice/Min"
          "MAurum/Code/Algebra/Morphism"
          "MAurum/Code/Algebra/Properties/BooleanAlgebra"
          "MAurum/Code/Algebra/Properties/DistributiveLattice"
          "MAurum/Code/Algebra/Properties/Lattice"
          "MAurum/Code/Algebra/Properties/Semilattice"
          "MAurum/Code/Algebra/Structures"
          "MAurum/Code/Algebra/Structures/Biased"
          "MAurum/Code/Algorithmic"
          "MAurum/Code/Algorithmic/Properties"
          "MAurum/Code/Algorithmic/CEKV"
          "MAurum/Code/Algorithmic/ReductionEC"
          "MAurum/Code/Algorithmic/CC"
          "MAurum/Code/Algorithmic/CK"
          "MAurum/Code/Algorithmic/Evaluation"
          "MAurum/Code/Algorithmic/Reduction"
          "MAurum/Code/Algorithmic/RenamingSubstitution"
          "MAurum/Code/Builtin"
          "MAurum/Code/Builtin/Constant/Term"
          "MAurum/Code/Builtin/Constant/Type"
          "MAurum/Code/Category/Applicative/Indexed"
          "MAurum/Code/Category/Functor"
          "MAurum/Code/Category/Monad/Indexed"
          "MAurum/Code/Check"
          "MAurum/Code/Data/Bool/Base"
          "MAurum/Code/Data/Bool/Properties"
          "MAurum/Code/Data/Char/Properties"
          "MAurum/Code/Data/Digit"
          "MAurum/Code/Data/Empty"
          "MAurum/Code/Data/Empty/Irrelevant"
          "MAurum/Code/Data/Empty/Polymorphic"
          "MAurum/Code/Data/Fin/Base"
          "MAurum/Code/Data/Integer"
          "MAurum/Code/Data/Integer/Base"
          "MAurum/Code/Data/Integer/Properties"
          "MAurum/Code/Data/Integer/Show"
          "MAurum/Code/Data/List/Base"
          "MAurum/Code/Data/List/Categorical"
          "MAurum/Code/Data/List/Extrema"
          "MAurum/Code/Data/List/Extrema/Core"
          "MAurum/Code/Data/List/Membership/DecSetoid"
          "MAurum/Code/Data/List/Membership/Propositional"
          "MAurum/Code/Data/List/Membership/Propositional/Properties"
          "MAurum/Code/Data/List/Membership/Propositional/Properties/Core"
          "MAurum/Code/Data/List/Membership/Setoid"
          "MAurum/Code/Data/List/Membership/Setoid/Properties"
          "MAurum/Code/Data/List/NonEmpty"
          "MAurum/Code/Data/List/Properties"
          "MAurum/Code/Data/List/Relation/Binary/Equality/Propositional"
          "MAurum/Code/Data/List/Relation/Binary/Equality/Setoid"
          "MAurum/Code/Data/List/Relation/Binary/Lex/Core"
          "MAurum/Code/Data/List/Relation/Binary/Lex/Strict"
          "MAurum/Code/Data/List/Relation/Binary/Pointwise"
          "MAurum/Code/Data/List/Relation/Binary/Pointwise/Properties"
          "MAurum/Code/Data/List/Relation/Unary/All"
          "MAurum/Code/Data/List/Relation/Unary/All/Properties"
          "MAurum/Code/Data/List/Relation/Unary/AllPairs/Core"
          "MAurum/Code/Data/List/Relation/Unary/Any"
          "MAurum/Code/Data/List/Relation/Unary/Any/Properties"
          "MAurum/Code/Data/Maybe/Base"
          "MAurum/Code/Data/Maybe/Relation/Unary/All"
          "MAurum/Code/Data/Maybe/Relation/Unary/Any"
          "MAurum/Code/Data/Nat/Base"
          "MAurum/Code/Data/Nat/DivMod"
          "MAurum/Code/Data/Nat/DivMod/Core"
          "MAurum/Code/Data/Nat/Divisibility/Core"
          "MAurum/Code/Data/Nat/Properties"
          "MAurum/Code/Data/Nat/Properties/Core"
          "MAurum/Code/Data/Nat/Show"
          "MAurum/Code/Data/Product"
          "MAurum/Code/Data/Product/Function/Dependent/Propositional"
          "MAurum/Code/Data/Product/Function/NonDependent/Propositional"
          "MAurum/Code/Data/Product/Function/NonDependent/Setoid"
          "MAurum/Code/Data/Product/Properties"
          "MAurum/Code/Data/Product/Relation/Binary/Pointwise/NonDependent"
          "MAurum/Code/Data/Sign/Base"
          "MAurum/Code/Data/String/Base"
          "MAurum/Code/Data/String/Properties"
          "MAurum/Code/Data/Sum/Base"
          "MAurum/Code/Data/Sum/Function/Propositional"
          "MAurum/Code/Data/Sum/Function/Setoid"
          "MAurum/Code/Data/Sum/Relation/Binary/Pointwise"
          "MAurum/Code/Data/These/Base"
          "MAurum/Code/Data/Vec/Base"
          "MAurum/Code/Data/Vec/Bounded/Base"
          "MAurum/Code/Debug/Trace"
          "MAurum/Code/Function/Bijection"
          "MAurum/Code/Function/Bundles"
          "MAurum/Code/Function/Equality"
          "MAurum/Code/Function/Equivalence"
          "MAurum/Code/Function/HalfAdjointEquivalence"
          "MAurum/Code/Function/Injection"
          "MAurum/Code/Function/Inverse"
          "MAurum/Code/Function/LeftInverse"
          "MAurum/Code/Function/Metric/Nat/Bundles"
          "MAurum/Code/Function/Metric/Structures"
          "MAurum/Code/Function/Related"
          "MAurum/Code/Function/Related/TypeIsomorphisms"
          "MAurum/Code/Function/Structures"
          "MAurum/Code/Function/Surjection"
          "MAurum/Code/IO/Primitive"
          "MAurum/Code/Induction"
          "MAurum/Code/Induction/WellFounded"
          "MAurum/Code/Level"
          "MAurum/Code/Raw"
          "MAurum/Code/Relation/Binary/Bundles"
          "MAurum/Code/Relation/Binary/Consequences"
          "MAurum/Code/Relation/Binary/Construct/Converse"
          "MAurum/Code/Relation/Binary/Construct/NaturalOrder/Left"
          "MAurum/Code/Relation/Binary/Construct/NonStrictToStrict"
          "MAurum/Code/Relation/Binary/Construct/On"
          "MAurum/Code/Relation/Binary/Definitions"
          "MAurum/Code/Relation/Binary/Indexed/Heterogeneous/Bundles"
          "MAurum/Code/Relation/Binary/Indexed/Heterogeneous/Construct/Trivial"
          "MAurum/Code/Relation/Binary/Indexed/Heterogeneous/Structures"
          "MAurum/Code/Relation/Binary/Lattice"
          "MAurum/Code/Relation/Binary/Properties/Poset"
          "MAurum/Code/Relation/Binary/Properties/Preorder"
          "MAurum/Code/Relation/Binary/PropositionalEquality"
          "MAurum/Code/Relation/Binary/PropositionalEquality/Algebra"
          "MAurum/Code/Relation/Binary/PropositionalEquality/Core"
          "MAurum/Code/Relation/Binary/PropositionalEquality/Properties"
          "MAurum/Code/Relation/Binary/HeterogeneousEquality/Core"
          "MAurum/Code/Relation/Binary/Reasoning/Base/Double"
          "MAurum/Code/Relation/Binary/Reasoning/Base/Triple"
          "MAurum/Code/Relation/Binary/Structures"
          "MAurum/Code/Relation/Nullary"
          "MAurum/Code/Relation/Nullary/Decidable"
          "MAurum/Code/Relation/Nullary/Decidable/Core"
          "MAurum/Code/Relation/Nullary/Negation"
          "MAurum/Code/Relation/Nullary/Product"
          "MAurum/Code/Relation/Nullary/Reflects"
          "MAurum/Code/Relation/Nullary/Sum"
          "MAurum/Code/Relation/Unary/Properties"
          "MAurum/Code/Scoped"
          "MAurum/Code/Scoped/Extrication"
          "MAurum/Code/Type"
          "MAurum/Code/Type/BetaNBE"
          "MAurum/Code/Type/BetaNBE/Completeness"
          "MAurum/Code/Type/BetaNBE/RenamingSubstitution"
          "MAurum/Code/Type/BetaNBE/Soundness"
          "MAurum/Code/Type/BetaNormal"
          "MAurum/Code/Type/Equality"
          "MAurum/Code/Type/RenamingSubstitution"
          "MAurum/Code/Utils"
          "MAurum/RTE"
          "MAurum/Code/Algorithmic/Erasure"
          "MAurum/Code/Declarative"
          "MAurum/Code/Untyped"
          "MAurum/Code/Untyped/Reduction"
          "MAurum/Code/Untyped/RenamingSubstitution"
          "MAurum/Code/Agda/Builtin/Maybe"
          "MAurum/Code/Algebra/Construct/NaturalChoice/Base"
          "MAurum/Code/Algebra/Construct/NaturalChoice/Max"
          "MAurum/Code/Algebra/Construct/NaturalChoice/MaxOp"
          "MAurum/Code/Algebra/Construct/NaturalChoice/MinMaxOp"
          "MAurum/Code/Algebra/Construct/NaturalChoice/MinOp"
          "MAurum/Code/Category/Comonad"
          "MAurum/Code/Data/List/NonEmpty/Base"
          "MAurum/Code/Data/List/Relation/Binary/Lex"
          "MAurum/Code/Data/List/Relation/Binary/Pointwise/Base"
          "MAurum/Code/Data/Maybe/Categorical"
          "MAurum/Code/Function/Base"
          "MAurum/Code/Function/Identity/Categorical"
          "MAurum/Code/Relation/Binary/Construct/Closure/Reflexive"
          "MAurum/Code/Relation/Binary/Construct/Closure/Reflexive/Properties"
          "MAurum/Code/Relation/Binary/Reasoning/Base/Single"
          "MAurum/Code/Relation/Binary/Reasoning/Setoid"
          "MAurum/Code/Relation/Nullary/Negation/Core"
          "Opts"
          "Raw"
          "Scoped"
          "Untyped"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "plc-agda" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."zerepoch-metatheory" or (errorHandler.buildDepError "zerepoch-metatheory"))
            ];
          buildable = true;
          hsSourceDirs = [ "exe" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "test1" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."zerepoch-metatheory" or (errorHandler.buildDepError "zerepoch-metatheory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.zerepoch-core.components.exes.plc or (pkgs.buildPackages.plc or (errorHandler.buildToolDepError "zerepoch-core:plc")))
            (hsPkgs.buildPackages.zerepoch-core.components.exes.uplc or (pkgs.buildPackages.uplc or (errorHandler.buildToolDepError "zerepoch-core:uplc")))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "TestSimple.hs" ];
          };
        "test2" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."zerepoch-metatheory" or (errorHandler.buildDepError "zerepoch-metatheory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.zerepoch-core.components.exes.plc or (pkgs.buildPackages.plc or (errorHandler.buildToolDepError "zerepoch-core:plc")))
            (hsPkgs.buildPackages.zerepoch-core.components.exes.uplc or (pkgs.buildPackages.uplc or (errorHandler.buildToolDepError "zerepoch-core:uplc")))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          };
        "test3" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."lazy-search" or (errorHandler.buildDepError "lazy-search"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."zerepoch-metatheory" or (errorHandler.buildDepError "zerepoch-metatheory"))
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."size-based" or (errorHandler.buildDepError "size-based"))
            (hsPkgs."Stream" or (errorHandler.buildDepError "Stream"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "TestNEAT.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../zerepoch-metatheory; }