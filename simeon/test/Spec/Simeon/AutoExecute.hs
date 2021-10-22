{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -w #-}
module Spec.Simeon.AutoExecute
    ( tests
    )
where

import           Control.Exception                     (SomeException, catch)
import           Control.Monad                         (void)
import qualified Data.Map.Strict                       as Map
import           Data.Maybe                            (isJust)
import qualified Data.Text                             as T
import qualified Data.Text.IO                          as T
import           Data.Text.Lazy                        (toStrict)
import           Language.Simeon.Analysis.FSSemantics
import           Language.Simeon.Client
import           Language.Simeon.Semantics
import           Language.Simeon.Util
import           System.IO.Unsafe                      (unsafePerformIO)

import           Data.Aeson                            (decode, encode)
import           Data.Aeson.Text                       (encodeToLazyText)
import qualified Data.ByteString                       as BS
import           Data.Either                           (isRight)
import           Data.Ratio                            ((%))
import           Data.String

import qualified Codec.CBOR.Write                      as Write
import qualified Codec.Serialise                       as Serialise
import           Language.Haskell.Interpreter          (Extension (OverloadedStrings), MonadInterpreter,
                                                        OptionVal ((:=)), as, interpret, languageExtensions,
                                                        runInterpreter, set, setImports)
import           Zerepoch.Contract.Test                  as T
import qualified Zerepoch.Trace.Emulator                 as Trace
import qualified ZerepochTx.AssocMap                     as AssocMap
import           ZerepochTx.Lattice

import           Ledger                                hiding (Value)
import qualified Ledger
import           Ledger.Bcc                            (entropicValueOf)
import           Ledger.Typed.Scripts                  (validatorScript)
import qualified ZerepochTx.Prelude                      as P
import           Spec.Simeon.Common
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant if" -}

tests :: TestTree
tests = testGroup "Simeon Auto Execution"
    [ awaitUntilTimeoutTest
    , autoexecZCBTest
    , autoexecZCBTestAliceWalksAway
    , autoexecZCBTestBobWalksAway
    ]


alice, bob, carol :: Wallet
alice = T.w1
bob = T.w2
carol = T.w3

-- Leave some entropic for fees
almostAll :: Ledger.Value
almostAll = defaultEntropicAmount <> P.inv (entropicValueOf 50)

autoexecZCBTest :: TestTree
autoexecZCBTest = checkPredicate "ZCB Auto Execute Contract"
    (assertNoFailedTransactions
    -- /\ emulatorLog (const False) ""
    T..&&. assertNotDone simeonZerepochContract (Trace.walletInstanceTag alice) "contract should not have any errors"
    T..&&. assertNotDone simeonZerepochContract (Trace.walletInstanceTag bob) "contract should not have any errors"
    T..&&. walletFundsChange alice (entropicValueOf 150)
    T..&&. walletFundsChange bob (entropicValueOf (-150))
    ) $ do

    bobHdl <- Trace.activateContractWallet bob simeonZerepochContract
    aliceHdl <- Trace.activateContractWallet alice simeonZerepochContract

    -- Bob will wait for the contract to appear on chain
    Trace.callEndpoint @"auto" bobHdl (params, bobPk, contractLifespan)

    -- Init a contract
    Trace.callEndpoint @"create" aliceHdl (AssocMap.empty, zeroCouponBond)
    Trace.waitNSlots 1

    -- Move all Alice's money to Carol, so she can't make a payment
    Trace.payToWallet alice carol almostAll
    Trace.waitNSlots 1

    Trace.callEndpoint @"auto" aliceHdl (params, alicePk, contractLifespan)
    Trace.waitNSlots 1

    -- Return money to Alice
    Trace.payToWallet carol alice almostAll
    Trace.waitNSlots 1

    -- Now Alice should be able to retry and pay to Bob
    void $ Trace.waitNSlots 2


autoexecZCBTestAliceWalksAway :: TestTree
autoexecZCBTestAliceWalksAway = checkPredicate
    "ZCB Auto Execute Contract when Alice walks away"
    (assertNoFailedTransactions
    -- /\ emulatorLog (const False) ""
    T..&&. assertNotDone simeonZerepochContract (Trace.walletInstanceTag alice) "contract should not have any errors"
    T..&&. assertNotDone simeonZerepochContract (Trace.walletInstanceTag bob) "contract should not have any errors"
    T..&&. walletFundsChange alice (P.inv almostAll)
    T..&&. walletFundsChange carol almostAll
    ) $ do
    bobHdl <- Trace.activateContractWallet bob simeonZerepochContract
    aliceHdl <- Trace.activateContractWallet alice simeonZerepochContract

    -- Bob will wait for the contract to appear on chain
    Trace.callEndpoint @"auto" bobHdl (params, bobPk, contractLifespan)

    -- Init a contract
    Trace.callEndpoint @"create" aliceHdl (AssocMap.empty, zeroCouponBond)
    Trace.waitNSlots 1

    -- Move all Alice's money to Carol, so she can't make a payment
    Trace.payToWallet alice carol almostAll
    Trace.waitNSlots 1

    Trace.callEndpoint @"auto" aliceHdl (params, alicePk, contractLifespan)
    Trace.waitNSlots 1
    Trace.waitNSlots 20
    -- Here Alice deposit timeout happened, so Bob should Close the contract
    void $ Trace.waitNSlots 1


autoexecZCBTestBobWalksAway :: TestTree
autoexecZCBTestBobWalksAway = checkPredicate
    "ZCB Auto Execute Contract when Bob walks away"
    (assertNoFailedTransactions
    -- /\ emulatorLog (const False) ""
    T..&&. assertNotDone simeonZerepochContract (Trace.walletInstanceTag alice) "contract should not have any errors"
    T..&&. assertNotDone simeonZerepochContract (Trace.walletInstanceTag bob) "contract should not have any errors"
    T..&&. walletFundsChange alice (entropicValueOf (-850))
    T..&&. walletFundsChange carol almostAll
    ) $ do
    bobHdl <- Trace.activateContractWallet bob simeonZerepochContract
    aliceHdl <- Trace.activateContractWallet alice simeonZerepochContract

    -- Bob will wait for the contract to appear on chain
    Trace.callEndpoint @"auto" bobHdl (params, bobPk, contractLifespan)

    -- Init a contract
    Trace.callEndpoint @"create" aliceHdl (AssocMap.empty, zeroCouponBond)
    Trace.waitNSlots 1

    Trace.payToWallet bob carol almostAll
    Trace.waitNSlots 1

    Trace.callEndpoint @"auto" aliceHdl (params, alicePk, contractLifespan)
    Trace.waitNSlots 1 -- Alice pays to Bob
    Trace.waitNSlots 15 -- Bob can't pay back
    Trace.waitNSlots 15 -- Bob can't pay back
    void $ Trace.waitNSlots 15 -- Bob can't pay back, walks away


awaitUntilTimeoutTest :: TestTree
awaitUntilTimeoutTest = checkPredicate "Party waits for contract to appear on chain until timeout"
    (assertNoFailedTransactions
    -- /\ emulatorLog (const False) ""
    T..&&. assertNotDone simeonZerepochContract (Trace.walletInstanceTag bob) "contract should close"
    ) $ do

    bobHdl <- Trace.activateContractWallet bob simeonZerepochContract
    aliceHdl <- Trace.activateContractWallet alice simeonZerepochContract

    -- Bob will wait for the contract to appear on chain
    Trace.callEndpoint @"auto" bobHdl (params, bobPk, contractLifespan)

    Trace.waitNSlots 15
    Trace.waitNSlots 15
    -- here Bob gets Timeout and closes the contract
    void $ Trace.waitNSlots 15

alicePk = PK (pubKeyHash $ walletPubKey alice)
bobPk = PK (pubKeyHash $ walletPubKey bob)

params = defaultSimeonParams

zeroCouponBond = When [ Case
        (Deposit alicePk alicePk bcc (Constant 850))
        (Pay alicePk (Party bobPk) bcc (Constant 850)
            (When
                [ Case (Deposit alicePk bobPk bcc (Constant 1000)) Close] 40 Close
            ))] 20 Close

contractLifespan = contractLifespanUpperBound zeroCouponBond

defaultEntropicAmount :: Ledger.Value
defaultEntropicAmount = defaultDist Map.! alice
