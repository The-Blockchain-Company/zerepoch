{-# LANGUAGE OverloadedStrings #-}
module Spec.Uniswap(
    tests
    ) where

import           Zerepoch.Contract.Test
import qualified Zerepoch.Contracts.Uniswap.Trace as Uniswap
import qualified Zerepoch.Trace.Emulator          as Trace

import           Test.Tasty

tests :: TestTree
tests = testGroup "uniswap" [
    checkPredicate "can create a liquidity pool and add liquidity"
        (assertNotDone Uniswap.setupTokens
                       (Trace.walletInstanceTag w1)
                       "setupTokens contract should be still running"
        .&&. assertNoFailedTransactions)
        Uniswap.uniswapTrace
    ]
