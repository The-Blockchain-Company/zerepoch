{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TransformSpec (transform) where

import           Common
import           TestLib

import           ZerepochCore.Quote

import qualified ZerepochCore                         as PLC
import qualified ZerepochCore.Pretty                  as PLC

import qualified ZerepochIR.Analysis.RetainedSize     as RetainedSize
import           ZerepochIR.Parser
import qualified ZerepochIR.Transform.Beta            as Beta
import qualified ZerepochIR.Transform.DeadCode        as DeadCode
import qualified ZerepochIR.Transform.Inline          as Inline
import qualified ZerepochIR.Transform.LetFloat        as LetFloat
import qualified ZerepochIR.Transform.LetMerge        as LetMerge
import qualified ZerepochIR.Transform.NonStrict       as NonStrict
import qualified ZerepochIR.Transform.RecSplit        as RecSplit
import           ZerepochIR.Transform.Rename          ()
import qualified ZerepochIR.Transform.ThunkRecursions as ThunkRec
import qualified ZerepochIR.Transform.Unwrap          as Unwrap

import           Control.Monad
import           Text.Megaparsec.Pos


transform :: TestNested
transform = testNested "transform" [
    thunkRecursions
    , nonStrict
    , letFloat
    , recSplit
    , inline
    , beta
    , unwrapCancel
    , deadCode
    , retainedSize
    , rename
    ]

thunkRecursions :: TestNested
thunkRecursions = testNested "thunkRecursions"
    $ map (goldenPir ThunkRec.thunkRecursions $ term @PLC.DefaultUni @PLC.DefaultFun)
    [ "listFold"
    , "monoMap"
    ]

nonStrict :: TestNested
nonStrict = testNested "nonStrict"
    $ map (goldenPir (runQuote . NonStrict.compileNonStrictBindings False) $ term @PLC.DefaultUni @PLC.DefaultFun)
    [ "nonStrict1"
    ]

letFloat :: TestNested
letFloat =
    testNested "letFloat"
    $ map (goldenPir (LetMerge.letMerge . RecSplit.recSplit . LetFloat.floatTerm . runQuote . PLC.rename) $ term @PLC.DefaultUni @PLC.DefaultFun)
  [ "letInLet"
  ,"listMatch"
  ,"maybe"
  ,"ifError"
  ,"mutuallyRecursiveTypes"
  ,"mutuallyRecursiveValues"
  ,"nonrec1"
  ,"nonrec2"
  ,"nonrec3"
  ,"nonrec4"
  ,"nonrec6"
  ,"nonrec7"
  ,"nonrec8"
  ,"nonrec9"
  ,"rec1"
  ,"rec2"
  ,"rec3"
  ,"rec4"
  ,"nonrecToRec"
  ,"nonrecToNonrec"
  ,"oldLength"
  ,"strictValue"
  ,"strictNonValue"
  ,"strictNonValue2"
  ,"strictNonValue3"
  ,"strictValueNonValue"
  ,"strictValueValue"
  ,"even3Eval"
  ,"strictNonValueDeep"
  ,"oldFloatBug"
  ,"outRhs"
  ,"outLam"
  ,"inLam"
  ,"rhsSqueezeVsNest"
  ]

recSplit :: TestNested
recSplit =
    testNested "recSplit"
    $ map (goldenPir (RecSplit.recSplit . runQuote . PLC.rename) $ term @PLC.DefaultUni @PLC.DefaultFun)
  [
    "truenonrec"
  , "mutuallyRecursiveTypes"
  , "mutuallyRecursiveValues"
  , "selfrecursive"
  , "small"
  , "big"
  ]


instance Semigroup SourcePos where
  p1 <> _ = p1

instance Monoid SourcePos where
  mempty = initialPos ""

inline :: TestNested
inline =
    testNested "inline"
    $ map (goldenPir (runQuote . (Inline.inline <=< PLC.rename)) $ term @PLC.DefaultUni @PLC.DefaultFun)
    [ "var"
    , "builtin"
    , "constant"
    , "transitive"
    , "tyvar"
    ]


beta :: TestNested
beta =
    testNested "beta"
    $ map (goldenPir (Beta.beta . runQuote . PLC.rename) $ term @PLC.DefaultUni @PLC.DefaultFun)
    [ "lamapp"
    , "absapp"
    ]

unwrapCancel :: TestNested
unwrapCancel =
    testNested "unwrapCancel"
    $ map (goldenPir Unwrap.unwrapCancel $ term @PLC.DefaultUni @PLC.DefaultFun)
    -- Note: these examples don't typecheck, but we don't care
    [ "unwrapWrap"
    , "wrapUnwrap"
    ]

deadCode :: TestNested
deadCode =
    testNested "deadCode"
    $ map (goldenPir (runQuote . DeadCode.removeDeadBindings) $ term @PLC.DefaultUni @PLC.DefaultFun)
    [ "typeLet"
    , "termLet"
    , "strictLet"
    , "nonstrictLet"
    , "datatypeLiveType"
    , "datatypeLiveConstr"
    , "datatypeLiveDestr"
    , "datatypeDead"
    , "singleBinding"
    , "builtinBinding"
    , "etaBuiltinBinding"
    , "nestedBindings"
    , "nestedBindingsIndirect"
    , "recBindingSimple"
    , "recBindingComplex"
    ]

retainedSize :: TestNested
retainedSize =
    testNested "retainedSize"
    $ map (goldenPir renameAndAnnotate $ term @PLC.DefaultUni @PLC.DefaultFun)
    [ "typeLet"
    , "termLet"
    , "strictLet"
    , "nonstrictLet"
    -- @Maybe@ is referenced, so it retains all of the data type.
    , "datatypeLiveType"
    -- @Nothing@ is referenced, so it retains all of the data type.
    , "datatypeLiveConstr"
    -- @match_Maybe@ is referenced, so it retains all of the data type.
    , "datatypeLiveDestr"
    , "datatypeDead"
    , "singleBinding"
    , "builtinBinding"
    , "etaBuiltinBinding"
    , "etaBuiltinBindingUsed"
    , "nestedBindings"
    , "nestedBindingsIndirect"
    , "recBindingSimple"
    , "recBindingComplex"
    ] where
        displayAnnsConfig = PLC.PrettyConfigClassic PLC.defPrettyConfigName True
        renameAndAnnotate
            = PLC.AttachPrettyConfig displayAnnsConfig
            . RetainedSize.annotateWithRetainedSize
            . runQuote
            . PLC.rename

rename :: TestNested
rename =
    testNested "rename"
    $ map (goldenPir (PLC.AttachPrettyConfig debugConfig . runQuote . PLC.rename) $ term @PLC.DefaultUni @PLC.DefaultFun)
    [ "allShadowedDataNonRec"
    , "allShadowedDataRec"
    , "paramShadowedDataNonRec"
    , "paramShadowedDataRec"
    ] where
        debugConfig = PLC.PrettyConfigClassic PLC.debugPrettyConfigName False
