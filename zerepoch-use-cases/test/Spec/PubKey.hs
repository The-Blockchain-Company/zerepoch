{-# LANGUAGE TypeApplications #-}
module Spec.PubKey(tests, pubKeyTrace) where

import           Control.Monad           (void)
import qualified Data.Map                as Map

import qualified Ledger
import qualified Ledger.Bcc              as Bcc
import           Ledger.Constraints      (ScriptLookups (..))
import qualified Ledger.Constraints      as Constraints
import           Ledger.Scripts          (unitRedeemer)
import           Ledger.Typed.Scripts    as Scripts
import           Zerepoch.Contract
import           Zerepoch.Contract.Test
import qualified Zerepoch.Trace.Emulator   as Trace

import           Zerepoch.Contracts.PubKey (PubKeyError, pubKeyContract)

import           Test.Tasty

theContract :: Contract () EmptySchema PubKeyError ()
theContract = do
  (txOutRef, ciTxOut, pkInst) <- pubKeyContract (Ledger.pubKeyHash $ walletPubKey w1) (Bcc.entropicValueOf 10)
  let lookups = ScriptLookups
                  { slMPS = Map.empty
                  , slTxOutputs = maybe mempty (Map.singleton txOutRef) ciTxOut
                  , slOtherScripts = Map.singleton (Scripts.validatorHash pkInst) (Scripts.validatorScript pkInst)
                  , slOtherData = Map.empty
                  , slPubKeyHashes = Map.empty
                  , slTypedValidator = Nothing
                  , slOwnPubkey = Nothing
                  }
  void $ submitTxConstraintsWith @Scripts.Any lookups (Constraints.mustSpendScriptOutput txOutRef unitRedeemer)

tests :: TestTree
tests = testGroup "pubkey"
  [ checkPredicate "works like a public key output"
      (walletFundsChange w1 mempty .&&. assertDone theContract (Trace.walletInstanceTag w1) (const True) "pubkey contract not done")
      pubKeyTrace
  ]

-- | Use 'pubKeyContract' to create a script output that works like a
--   public key output, requiring only the right signature on the spending
--   transaction. Then spend the script output.
pubKeyTrace :: Trace.EmulatorTrace ()
pubKeyTrace = do
    _ <- Trace.activateContractWallet w1 theContract
    void $ Trace.waitNSlots 2
