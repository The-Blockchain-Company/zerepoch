{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fplugin ZerepochTx.Plugin -fplugin-opt ZerepochTx.Plugin:defer-errors -fplugin-opt ZerepochTx.Plugin:no-context #-}

module StdLib.Spec where

import           Common
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Ratio                 ((%))
import           GHC.Real                   (reduce)
import           Hedgehog                   (MonadGen, Property)
import qualified Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range
import           Lib
import           PlcTestUtils
import           ZerepochCore.Data            (Data (..))
import           Test.Tasty                 (TestName)
import           Test.Tasty.Hedgehog        (testProperty)

import qualified ZerepochTx.Eq                as ZerepochTx
import qualified ZerepochTx.Ord               as ZerepochTx
import qualified ZerepochTx.Prelude           as ZerepochTx
import qualified ZerepochTx.Ratio             as Ratio

import           ZerepochTx.Builtins.Internal (BuiltinData (..))
import           ZerepochTx.Code
import qualified ZerepochTx.Lift              as Lift
import           ZerepochTx.Plugin

import qualified ZerepochCore.Data            as PLC

import           Data.Proxy

roundPlc :: CompiledCode (Ratio.Rational -> Integer)
roundPlc = plc (Proxy @"roundPlc") Ratio.round

tests :: TestNested
tests =
  testNested "StdLib"
    [ goldenUEval "ratioInterop" [ getPlc roundPlc, Lift.liftProgram (Ratio.fromGHC 3.75) ]
    , testRatioProperty "round" Ratio.round round
    , testRatioProperty "truncate" Ratio.truncate truncate
    , testRatioProperty "abs" (fmap Ratio.toGHC Ratio.abs) abs
    , pure $ testProperty "ord" testOrd
    , pure $ testProperty "divMod" testDivMod
    , pure $ testProperty "quotRem" testQuotRem
    , pure $ testProperty "reduce" testReduce
    , pure $ testProperty "Eq @Data" eqData
    , goldenPir "errorTrace" errorTrace
    ]

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x

-- | Evaluate (deeply, to get through tuples) a value, throwing away any exception and just representing it as 'Nothing'.
tryHard :: (MonadIO m, NFData a) => a -> m (Maybe a)
tryHard a = eitherToMaybe <$> (liftIO $ try @SomeException $ evaluate $ force a)

testRatioProperty :: (Show a, Eq a) => TestName -> (Ratio.Rational -> a) -> (Rational -> a) -> TestNested
testRatioProperty nm zerepochFunc ghcFunc = pure $ testProperty nm $ Hedgehog.property $ do
    rat <- Hedgehog.forAll $ Gen.realFrac_ (Range.linearFrac (-10000) 100000)
    let ghcResult = ghcFunc rat
        zerepochResult = zerepochFunc $ Ratio.fromGHC rat
    Hedgehog.annotateShow ghcResult
    Hedgehog.annotateShow zerepochResult
    Hedgehog.assert (ghcResult == zerepochResult)

testDivMod :: Property
testDivMod = Hedgehog.property $ do
    let gen = Gen.integral (Range.linear (-10000) 100000)
    (n1, n2) <- Hedgehog.forAll $ (,) <$> gen <*> gen
    ghcResult <- tryHard $ divMod n1 n2
    zerepochResult <- tryHard $ Ratio.divMod n1 n2
    Hedgehog.annotateShow ghcResult
    Hedgehog.annotateShow zerepochResult
    Hedgehog.assert (ghcResult == zerepochResult)

testQuotRem :: Property
testQuotRem = Hedgehog.property $ do
    let gen = Gen.integral (Range.linear (-10000) 100000)
    (n1, n2) <- Hedgehog.forAll $ (,) <$> gen <*> gen
    ghcResult <- tryHard $ quotRem n1 n2
    zerepochResult <- tryHard $ Ratio.quotRem n1 n2
    Hedgehog.annotateShow ghcResult
    Hedgehog.annotateShow zerepochResult
    Hedgehog.assert (ghcResult == zerepochResult)

testReduce :: Property
testReduce = Hedgehog.property $ do
    let gen = Gen.integral (Range.linear (-10000) 100000)
    (n1, n2) <- Hedgehog.forAll $ (,) <$> gen <*> gen
    ghcResult <- tryHard $ reduce n1 n2
    zerepochResult <- tryHard $ Ratio.toGHC $ Ratio.reduce n1 n2
    Hedgehog.annotateShow ghcResult
    Hedgehog.annotateShow zerepochResult
    Hedgehog.assert (ghcResult == zerepochResult)

testOrd :: Property
testOrd = Hedgehog.property $ do
    let gen = Gen.integral (Range.linear (-10000) 100000)
    n1 <- Hedgehog.forAll $ (%) <$> gen <*> gen
    n2 <- Hedgehog.forAll $ (%) <$> gen <*> gen
    ghcResult <- tryHard $ n1 <= n2
    zerepochResult <- tryHard $ (ZerepochTx.<=) (Ratio.fromGHC n1) (Ratio.fromGHC n2)
    Hedgehog.annotateShow ghcResult
    Hedgehog.annotateShow zerepochResult
    Hedgehog.assert (ghcResult == zerepochResult)

eqData :: Property
eqData = Hedgehog.property $ do
    theData <- BuiltinData <$> Hedgehog.forAll genData
    let ghcResult = theData == theData
        zerepochResult = theData ZerepochTx.== theData
    Hedgehog.annotateShow theData
    Hedgehog.assert (ghcResult && zerepochResult)

genData :: MonadGen m => m PLC.Data
genData =
    let genInteger = Gen.integral (Range.linear (-10000) 100000)
        genBytes   = Gen.bytes (Range.linear 0 1000)
        genList    = Gen.list (Range.linear 0 10)
    in Gen.recursive
            Gen.choice
            [ PLC.I <$> genInteger
            , PLC.B <$> genBytes
            ]
            [ PLC.Constr <$> genInteger <*> genList genData
            , PLC.Map <$> genList ((,) <$> genData <*> genData)
            , PLC.List <$> genList genData
            ]

errorTrace :: CompiledCode (Integer)
errorTrace = plc (Proxy @"errorTrace") (ZerepochTx.traceError "")
