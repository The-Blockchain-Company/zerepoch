{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
module Spec.ErrorHandling(tests) where

import           Control.Monad                  (void)
import           Data.Default                   (Default (def))
import           Zerepoch.Contract.Test

import           Zerepoch.Contracts.ErrorHandling
import qualified Zerepoch.Trace.Emulator          as Trace

import           Test.Tasty

tests :: TestTree
tests = testGroup "error handling"
    [ checkPredicate "throw an error"
        (assertContractError @_ @() (contract def) (Trace.walletInstanceTag w1) (\case { Error1 _ -> True; _ -> False}) "should throw error")
        $ do
            slotCfg <- Trace.getSlotConfig
            hdl <- Trace.activateContractWallet @_ @() @_ @MyError w1 (contract slotCfg)
            Trace.callEndpoint @"throwError" hdl ()
            void Trace.nextSlot

    , checkPredicate "catch an error"
        (assertDone @_ @() @_ @MyError (contract def) (Trace.walletInstanceTag w1) (const True) "should be done")
        $ do
            slotCfg <- Trace.getSlotConfig
            hdl <- Trace.activateContractWallet @_ @() @_ @MyError w1 (contract slotCfg)
            Trace.callEndpoint @"catchError" hdl ()
            void Trace.nextSlot
    ]
