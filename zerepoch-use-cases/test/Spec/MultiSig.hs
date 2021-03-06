{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
module Spec.MultiSig(tests, failingTrace, succeedingTrace) where

import           Control.Monad             (void)
import qualified Ledger
import qualified Ledger.Bcc                as Bcc
import           Ledger.Crypto             (privateKey1, privateKey2, privateKey3)
import           Ledger.Index              (ValidationError (ScriptFailure))
import           Ledger.Scripts            (ScriptError (EvaluationError))
import           Zerepoch.Contract           (Contract, ContractError)
import           Zerepoch.Contract.Test
import           Zerepoch.Contracts.MultiSig as MS
import           Zerepoch.Trace.Emulator     (EmulatorTrace)
import qualified Zerepoch.Trace.Emulator     as Trace
import qualified ZerepochTx                  as ZerepochTx
import           Prelude                   hiding (not)
import           Test.Tasty
import           Wallet.Emulator.Wallet    (signPrivateKeys)

tests :: TestTree
tests = testGroup "multisig"
    [ checkPredicate "2 out of 5"
        (assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["not enough signatures"] _) -> True; _ -> False  }))
        failingTrace

    , checkPredicate "3 out of 5"
        assertNoFailedTransactions
        succeedingTrace

    , goldenPir "test/Spec/multisig.pir" $$(ZerepochTx.compile [|| MS.validate ||])
    ]

-- | Lock some funds, then attempt to unlock them with a transaction
--   that doesn't have the required number of signatures
failingTrace :: EmulatorTrace ()
failingTrace = do
    hdl <- Trace.activateContractWallet w1 theContract
    Trace.callEndpoint @"lock" hdl (multiSig, Bcc.entropicValueOf 10)
    _ <- Trace.waitNSlots 1
    Trace.setSigningProcess w1 (signPrivateKeys [privateKey1, privateKey2])
    Trace.callEndpoint @"unlock" hdl (multiSig, fmap walletPubKey [w1, w2])
    void $ Trace.waitNSlots 1

-- | Lock some funds, then unlock them with a transaction that has the
--   three required signatures.
succeedingTrace :: EmulatorTrace ()
succeedingTrace = do
    hdl <- Trace.activateContractWallet w1 theContract
    Trace.callEndpoint @"lock" hdl (multiSig, Bcc.entropicValueOf 10)
    _ <- Trace.waitNSlots 1
    Trace.setSigningProcess w1 (signPrivateKeys [privateKey1, privateKey2, privateKey3])
    Trace.callEndpoint @"unlock" hdl (multiSig, fmap walletPubKey [w1, w2, w3])
    void $ Trace.waitNSlots 1

theContract :: Contract () MultiSigSchema ContractError ()
theContract = MS.contract

-- a 'MultiSig' contract that requires three out of five signatures
multiSig :: MultiSig
multiSig = MultiSig
        { signatories = Ledger.pubKeyHash . walletPubKey . knownWallet <$> [1..5]
        , minNumSignatures = 3
        }
