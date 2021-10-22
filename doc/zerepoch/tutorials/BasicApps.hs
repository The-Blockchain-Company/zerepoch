{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
module BasicApps where

-- BLOCK0

import           Control.Monad          (void)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Text              as T
import           GHC.Generics           (Generic)
import           Ledger
import qualified Ledger.Bcc             as Bcc
import qualified Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Zerepoch.Contract
import qualified ZerepochTx               as ZerepochTx
import           ZerepochTx.Prelude
import qualified Prelude                as Haskell
import           Schema
import           Wallet.Emulator.Wallet

-- BLOCK1

data SplitData =
    SplitData
        { recipient1 :: PubKeyHash -- ^ First recipient of the funds
        , recipient2 :: PubKeyHash -- ^ Second recipient of the funds
        , amount     :: Bcc -- ^ How much Bcc we want to lock
        }
    deriving stock (Haskell.Show, Generic)

-- For a 'real' application use 'makeIsDataIndexed' to ensure the output is stable over time
ZerepochTx.unstableMakeIsData ''SplitData
ZerepochTx.makeLift ''SplitData

-- BLOCK2

validateSplit :: SplitData -> () -> ScriptContext -> Bool
validateSplit SplitData{recipient1, recipient2, amount} _ ScriptContext{scriptContextTxInfo} =
    let half = Bcc.divide amount 2 in
    Bcc.fromValue (valuePaidTo scriptContextTxInfo recipient1) >= half &&
    Bcc.fromValue (valuePaidTo scriptContextTxInfo recipient2) >= (amount - half)

-- BLOCK3

data Split
instance Scripts.ValidatorTypes Split where
    type instance RedeemerType Split = ()
    type instance DatumType Split = SplitData

splitValidator :: Scripts.TypedValidator Split
splitValidator = Scripts.mkTypedValidator @Split
    $$(ZerepochTx.compile [|| validateSplit ||])
    $$(ZerepochTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @SplitData @()

-- BLOCK4

data LockArgs =
        LockArgs
            { recipient1Wallet :: Wallet
            , recipient2Wallet :: Wallet
            , totalBcc         :: Bcc
            }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

type SplitSchema =
        Endpoint "lock" LockArgs
        .\/ Endpoint "unlock" LockArgs

-- BLOCK5

lock :: Promise () SplitSchema T.Text ()
lock = endpoint @"lock" (lockFunds . mkSplitData)

unlock :: Promise () SplitSchema T.Text ()
unlock = endpoint @"unlock" (unlockFunds . mkSplitData)

-- BLOCK6

mkSplitData :: LockArgs -> SplitData
mkSplitData LockArgs{recipient1Wallet, recipient2Wallet, totalBcc} =
    let convert :: Wallet -> PubKeyHash
        convert = pubKeyHash . walletPubKey
    in
    SplitData
        { recipient1 = convert recipient1Wallet
        , recipient2 = convert recipient2Wallet
        , amount = totalBcc
        }

-- BLOCK7

lockFunds :: SplitData -> Contract () SplitSchema T.Text ()
lockFunds s@SplitData{amount} = do
    logInfo $ "Locking " <> Haskell.show amount
    let tx = Constraints.mustPayToTheScript s (Bcc.toValue amount)
    void $ submitTxConstraints splitValidator tx

-- BLOCK8

unlockFunds :: SplitData -> Contract () SplitSchema T.Text ()
unlockFunds SplitData{recipient1, recipient2, amount} = do
    let contractAddress = Scripts.validatorAddress splitValidator
    utxos <- utxosAt contractAddress
    let half = Bcc.divide amount 2
        tx =
            collectFromScript utxos ()
            <> Constraints.mustPayToPubKey recipient1 (Bcc.toValue half)
            <> Constraints.mustPayToPubKey recipient2 (Bcc.toValue $ amount - half)
    void $ submitTxConstraintsSpending splitValidator utxos tx

-- BLOCK9

endpoints :: Contract () SplitSchema T.Text ()
-- BLOCK10

endpoints = selectList [lock, unlock]

-- BLOCK11
