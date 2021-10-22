{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Zerepoch.PAB.Effects.Contract.BuiltinSpec
    ( tests
    ) where

import           Control.Monad                            (forM_)
import           Control.Monad.IO.Class                   (liftIO)
import           Data.UUID.V4                             (nextRandom)
import qualified Zerepoch.Contracts.PingPong                as Contracts.PingPong
import           Zerepoch.PAB.CoreSpec                      (assertEqual, runScenario)
import           Zerepoch.PAB.Effects.Contract.Builtin      (SomeBuiltin (..), SomeBuiltinState (..), fromResponse,
                                                           getResponse)
import           Zerepoch.PAB.Effects.Contract.ContractTest (TestContracts (..))
import           Zerepoch.PAB.Events.Contract               (ContractInstanceId (..))
import           Zerepoch.Trace.Emulator.Types
import           Test.Tasty                               (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit                         (testCase)
import           Wallet.Emulator.Wallet

tests :: TestTree
tests = testGroup "Zerepoch.PAB.Effects.Contract.Builtin" [stateTests]

stateTests :: TestTree
stateTests =
  testGroup "Builtin state tests"
    [ testCase "getResponse/fromResponse round trip empty state"
        $ runScenario
        $ do
          let c = Contracts.PingPong.combined
              s = emptyInstanceState c
              b = SomeBuiltin c

          let wIn = SomeBuiltinState s mempty
          cid <- ContractInstanceId <$> liftIO nextRandom

          let rIn = getResponse wIn
          wOut <- fromResponse @TestContracts cid b rIn

          let rOut = getResponse wOut
          assertEqual "responses equal" rIn rOut
    ]
