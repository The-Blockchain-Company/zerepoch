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
      identifier = { name = "zerepoch-core"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@blockchain-company.io";
      author = "Zerepoch Core Team";
      homepage = "";
      url = "";
      synopsis = "Language library for Zerepoch Core";
      description = "Pretty-printer, parser, and typechecker for Zerepoch Core.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "cost-model/data/builtinCostModel.json"
        "cost-model/data/cekMachineCosts.json"
        "cost-model/data/benching.csv"
        "cost-model/data/*.R"
        ];
      extraTmpFiles = [];
      extraDocFiles = [ "README.md" ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."Stream" or (errorHandler.buildDepError "Stream"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."algebraic-graphs" or (errorHandler.buildDepError "algebraic-graphs"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bcc-crypto" or (errorHandler.buildDepError "bcc-crypto"))
          (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."composition-prelude" or (errorHandler.buildDepError "composition-prelude"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."dependent-map" or (errorHandler.buildDepError "dependent-map"))
          (hsPkgs."dependent-sum-template" or (errorHandler.buildDepError "dependent-sum-template"))
          (hsPkgs."deriving-aeson" or (errorHandler.buildDepError "deriving-aeson"))
          (hsPkgs."deriving-compat" or (errorHandler.buildDepError "deriving-compat"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."dom-lt" or (errorHandler.buildDepError "dom-lt"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
          (hsPkgs."lazy-search" or (errorHandler.buildDepError "lazy-search"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
          (hsPkgs."monoidal-containers" or (errorHandler.buildDepError "monoidal-containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."prettyprinter-configurable" or (errorHandler.buildDepError "prettyprinter-configurable"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."recursion-schemes" or (errorHandler.buildDepError "recursion-schemes"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."size-based" or (errorHandler.buildDepError "size-based"))
          (hsPkgs."some" or (errorHandler.buildDepError "some"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
          (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."th-lift" or (errorHandler.buildDepError "th-lift"))
          (hsPkgs."th-lift-instances" or (errorHandler.buildDepError "th-lift-instances"))
          (hsPkgs."th-utilities" or (errorHandler.buildDepError "th-utilities"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."witherable" or (errorHandler.buildDepError "witherable"))
          (hsPkgs."word-array" or (errorHandler.buildDepError "word-array"))
          (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.alex.components.exes.alex or (pkgs.buildPackages.alex or (errorHandler.buildToolDepError "alex:alex")))
          (hsPkgs.buildPackages.happy.components.exes.happy or (pkgs.buildPackages.happy or (errorHandler.buildToolDepError "happy:happy")))
          ];
        buildable = true;
        modules = [
          "ZerepochCore/Analysis/Definitions"
          "ZerepochCore/Constant/Function"
          "ZerepochCore/Constant/Meaning"
          "ZerepochCore/Constant/Typed"
          "ZerepochCore/Core/Instance"
          "ZerepochCore/Core/Instance/Eq"
          "ZerepochCore/Core/Instance/Pretty"
          "ZerepochCore/Core/Instance/Pretty/Classic"
          "ZerepochCore/Core/Instance/Pretty/Common"
          "ZerepochCore/Core/Instance/Pretty/Default"
          "ZerepochCore/Core/Instance/Pretty/Plc"
          "ZerepochCore/Core/Instance/Pretty/Readable"
          "ZerepochCore/Core/Instance/Recursive"
          "ZerepochCore/Core/Instance/Scoping"
          "ZerepochCore/Core/Plated"
          "ZerepochCore/Core/Type"
          "ZerepochCore/DeBruijn/Internal"
          "ZerepochCore/Default/Builtins"
          "ZerepochCore/Default/Universe"
          "ZerepochCore/Eq"
          "ZerepochCore/Evaluation/Machine/ExBudgetingDefaults"
          "ZerepochCore/Generators/Internal/Denotation"
          "ZerepochCore/Generators/Internal/Dependent"
          "ZerepochCore/Generators/Internal/Entity"
          "ZerepochCore/Generators/Internal/TypeEvalCheck"
          "ZerepochCore/Generators/Internal/TypedBuiltinGen"
          "ZerepochCore/Generators/Internal/Utils"
          "ZerepochCore/Lexer/Type"
          "ZerepochCore/Parsable"
          "ZerepochCore/Parser/Internal"
          "ZerepochCore/ParserCommon"
          "ZerepochCore/Pretty/Classic"
          "ZerepochCore/Pretty/ConfigName"
          "ZerepochCore/Pretty/Default"
          "ZerepochCore/Pretty/Plc"
          "ZerepochCore/Pretty/PrettyConst"
          "ZerepochCore/Pretty/Readable"
          "ZerepochCore/Pretty/Utils"
          "ZerepochCore/Size"
          "ZerepochCore/TypeCheck"
          "ZerepochCore/TypeCheck/Internal"
          "ZerepochIR/Analysis/Dependencies"
          "ZerepochIR/Analysis/Size"
          "ZerepochIR/Analysis/Usages"
          "ZerepochIR/Compiler/Datatype"
          "ZerepochIR/Compiler/Error"
          "ZerepochIR/Compiler/Let"
          "ZerepochIR/Compiler/Lower"
          "ZerepochIR/Compiler/Provenance"
          "ZerepochIR/Compiler/Recursion"
          "ZerepochIR/Compiler/Types"
          "ZerepochIR/Normalize"
          "ZerepochIR/TypeCheck/Internal"
          "UntypedZerepochCore/Analysis/Definitions"
          "UntypedZerepochCore/Core"
          "UntypedZerepochCore/Core/Instance"
          "UntypedZerepochCore/Core/Instance/Eq"
          "UntypedZerepochCore/Core/Instance/Flat"
          "UntypedZerepochCore/Core/Instance/Pretty"
          "UntypedZerepochCore/Core/Instance/Pretty/Classic"
          "UntypedZerepochCore/Core/Instance/Pretty/Default"
          "UntypedZerepochCore/Core/Instance/Pretty/Plc"
          "UntypedZerepochCore/Core/Instance/Pretty/Readable"
          "UntypedZerepochCore/Core/Instance/Recursive"
          "UntypedZerepochCore/Core/Plated"
          "UntypedZerepochCore/Evaluation/Machine/Cek/CekMachineCosts"
          "UntypedZerepochCore/Evaluation/Machine/Cek/ExBudgetMode"
          "UntypedZerepochCore/Evaluation/Machine/Cek/EmitterMode"
          "UntypedZerepochCore/Mark"
          "UntypedZerepochCore/Rename/Internal"
          "UntypedZerepochCore/Size"
          "UntypedZerepochCore/Subst"
          "UntypedZerepochCore/Transform/Simplify"
          "Data/Aeson/Flatten"
          "Data/Aeson/THReader"
          "Data/Functor/Foldable/Monadic"
          "Universe/Core"
          "ZerepochCore"
          "ZerepochCore/Check/Normal"
          "ZerepochCore/Check/Scoping"
          "ZerepochCore/Check/Uniques"
          "ZerepochCore/Check/Value"
          "ZerepochCore/Constant"
          "ZerepochCore/Constant/Dynamic/Emit"
          "ZerepochCore/Core"
          "ZerepochCore/Data"
          "ZerepochCore/DataFilePaths"
          "ZerepochCore/DeBruijn"
          "ZerepochCore/Default"
          "ZerepochCore/Error"
          "ZerepochCore/Evaluation/Machine/BuiltinCostModel"
          "ZerepochCore/Evaluation/Machine/Ck"
          "ZerepochCore/Evaluation/Machine/CostModelInterface"
          "ZerepochCore/Evaluation/Machine/ExBudget"
          "ZerepochCore/Evaluation/Machine/ExMemory"
          "ZerepochCore/Evaluation/Machine/Exception"
          "ZerepochCore/Evaluation/Machine/MachineParameters"
          "ZerepochCore/Evaluation/Result"
          "ZerepochCore/Examples/Builtins"
          "ZerepochCore/Examples/Data/Data"
          "ZerepochCore/Examples/Data/InterList"
          "ZerepochCore/Examples/Data/List"
          "ZerepochCore/Examples/Data/Pair"
          "ZerepochCore/Examples/Data/Shad"
          "ZerepochCore/Examples/Data/TreeForest"
          "ZerepochCore/Examples/Data/Vec"
          "ZerepochCore/Examples/Everything"
          "ZerepochCore/Flat"
          "ZerepochCore/FsTree"
          "ZerepochCore/Generators"
          "ZerepochCore/Generators/AST"
          "ZerepochCore/Generators/Interesting"
          "ZerepochCore/Generators/NEAT/Common"
          "ZerepochCore/Generators/NEAT/Spec"
          "ZerepochCore/Generators/NEAT/Term"
          "ZerepochCore/Generators/NEAT/Type"
          "ZerepochCore/Generators/Test"
          "ZerepochCore/Lexer"
          "ZerepochCore/Mark"
          "ZerepochCore/MkPlc"
          "ZerepochCore/Name"
          "ZerepochCore/Normalize"
          "ZerepochCore/Normalize/Internal"
          "ZerepochCore/Parser"
          "ZerepochCore/Pretty"
          "ZerepochCore/Quote"
          "ZerepochCore/Rename"
          "ZerepochCore/Rename/Internal"
          "ZerepochCore/Rename/Monad"
          "ZerepochCore/StdLib/Data/Bool"
          "ZerepochCore/StdLib/Data/ChurchNat"
          "ZerepochCore/StdLib/Data/Data"
          "ZerepochCore/StdLib/Data/Function"
          "ZerepochCore/StdLib/Data/Integer"
          "ZerepochCore/StdLib/Data/List"
          "ZerepochCore/StdLib/Data/Nat"
          "ZerepochCore/StdLib/Data/Pair"
          "ZerepochCore/StdLib/Data/ScottList"
          "ZerepochCore/StdLib/Data/ScottUnit"
          "ZerepochCore/StdLib/Data/Sum"
          "ZerepochCore/StdLib/Data/Unit"
          "ZerepochCore/StdLib/Everything"
          "ZerepochCore/StdLib/Meta"
          "ZerepochCore/StdLib/Meta/Data/Function"
          "ZerepochCore/StdLib/Meta/Data/Tuple"
          "ZerepochCore/StdLib/Type"
          "ZerepochCore/Subst"
          "ZerepochIR"
          "ZerepochIR/Analysis/RetainedSize"
          "ZerepochIR/Compiler"
          "ZerepochIR/Compiler/Definitions"
          "ZerepochIR/Compiler/Names"
          "ZerepochIR/Core"
          "ZerepochIR/Core/Instance"
          "ZerepochIR/Core/Instance/Flat"
          "ZerepochIR/Core/Instance/Pretty"
          "ZerepochIR/Core/Instance/Scoping"
          "ZerepochIR/Core/Plated"
          "ZerepochIR/Core/Type"
          "ZerepochIR/Error"
          "ZerepochIR/Generators/AST"
          "ZerepochIR/Mark"
          "ZerepochIR/MkPir"
          "ZerepochIR/Parser"
          "ZerepochIR/Purity"
          "ZerepochIR/Subst"
          "ZerepochIR/Transform/Beta"
          "ZerepochIR/Transform/DeadCode"
          "ZerepochIR/Transform/Inline"
          "ZerepochIR/Transform/LetFloat"
          "ZerepochIR/Transform/LetMerge"
          "ZerepochIR/Transform/RecSplit"
          "ZerepochIR/Transform/NonStrict"
          "ZerepochIR/Transform/Rename"
          "ZerepochIR/Transform/Substitute"
          "ZerepochIR/Transform/ThunkRecursions"
          "ZerepochIR/Transform/Unwrap"
          "ZerepochIR/TypeCheck"
          "UntypedZerepochCore"
          "UntypedZerepochCore/DeBruijn"
          "UntypedZerepochCore/Evaluation/HOAS"
          "UntypedZerepochCore/Evaluation/Machine/Cek"
          "UntypedZerepochCore/Evaluation/Machine/Cek/Internal"
          "UntypedZerepochCore/Parser"
          "UntypedZerepochCore/Rename"
          "UntypedZerepochCore/Check/Uniques"
          "UntypedZerepochCore/Core/Type"
          "Common"
          "Crypto"
          "Data/ByteString/Hash"
          "Data/SatInt"
          "Data/Text/Prettyprint/Doc/Custom"
          "ErrorCode"
          "PlcTestUtils"
          "ZerepochPrelude"
          "Universe"
          ];
        hsSourceDirs = [
          "zerepoch-core/src"
          "zerepoch-core/stdlib"
          "zerepoch-core/examples"
          "zerepoch-ir/src"
          "untyped-zerepoch-core/src"
          "generators"
          "prelude"
          "common"
          ];
        };
      sublibs = {
        "index-envs" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."ral" or (errorHandler.buildDepError "ral"))
            ];
          buildable = true;
          modules = [ "Data/DeBruijnEnv" "Data/RandomAccessList/SkewBinary" ];
          hsSourceDirs = [ "index-envs/src" ];
          };
        };
      exes = {
        "plc" = {
          depends = [
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."monoidal-containers" or (errorHandler.buildDepError "monoidal-containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          modules = [ "Common" "Parsers" ];
          hsSourceDirs = [ "executables" ];
          mainPath = [ "plc/Main.hs" ];
          };
        "uplc" = {
          depends = [
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."monoidal-containers" or (errorHandler.buildDepError "monoidal-containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          modules = [ "Common" "Parsers" ];
          hsSourceDirs = [ "executables" ];
          mainPath = [ "uplc/Main.hs" ];
          };
        "pir" = {
          depends = [
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          hsSourceDirs = [ "executables" ];
          mainPath = [ "pir/Main.hs" ];
          };
        };
      tests = {
        "satint-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            ];
          buildable = true;
          hsSourceDirs = [ "zerepoch-core/satint-test" ];
          mainPath = [ "TestSatInt.hs" ];
          };
        "zerepoch-core-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          modules = [
            "Check/Spec"
            "CostModelInterface/Spec"
            "Evaluation/Machines"
            "Evaluation/Spec"
            "Names/Spec"
            "Normalization/Check"
            "Normalization/Type"
            "Pretty/Readable"
            "TypeSynthesis/Spec"
            ];
          hsSourceDirs = [ "zerepoch-core/test" ];
          mainPath = [ "Spec.hs" ];
          };
        "zerepoch-ir-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [
            "NamesSpec"
            "ParserSpec"
            "TestLib"
            "TransformSpec"
            "TypeSpec"
            ];
          hsSourceDirs = [ "zerepoch-ir/test" ];
          mainPath = [ "Spec.hs" ];
          };
        "untyped-zerepoch-core-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [
            "Evaluation/Builtins"
            "Evaluation/Builtins/Common"
            "Evaluation/Builtins/Definition"
            "Evaluation/Builtins/MakeRead"
            "Evaluation/Golden"
            "Evaluation/Machines"
            "Transform/Simplify"
            ];
          hsSourceDirs = [ "untyped-zerepoch-core/test" ];
          mainPath = [ "Spec.hs" ];
          };
        "index-envs-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."zerepoch-core".components.sublibs.index-envs or (errorHandler.buildDepError "zerepoch-core:index-envs"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          hsSourceDirs = [ "index-envs/test" ];
          mainPath = [ "TestRAList.hs" ];
          };
        };
      benchmarks = {
        "cost-model-budgeting-bench" = {
          depends = [
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."criterion-measurement" or (errorHandler.buildDepError "criterion-measurement"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            ];
          buildable = true;
          modules = [ "CriterionExtensions" "Nops" ];
          hsSourceDirs = [ "cost-model/budgeting-bench" ];
          };
        "update-cost-model" = {
          depends = [
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."inline-r" or (errorHandler.buildDepError "inline-r"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          modules = [ "CostModelCreation" ];
          hsSourceDirs = [ "cost-model/create-cost-model" ];
          };
        "cost-model-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."zerepoch-core" or (errorHandler.buildDepError "zerepoch-core"))
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."inline-r" or (errorHandler.buildDepError "inline-r"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          modules = [ "CostModelCreation" ];
          hsSourceDirs = [ "cost-model/test" "cost-model/create-cost-model" ];
          };
        "index-envs-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."zerepoch-core".components.sublibs.index-envs or (errorHandler.buildDepError "zerepoch-core:index-envs"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."ral" or (errorHandler.buildDepError "ral"))
            ];
          buildable = true;
          hsSourceDirs = [ "index-envs/bench" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../zerepoch-core; }