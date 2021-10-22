{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module BasicPolicies where

import qualified ZerepochCore.Default   as PLC
import           ZerepochTx
import           ZerepochTx.Lift
import           ZerepochTx.Prelude

import           Ledger
import           Ledger.Bcc
import           Ledger.Typed.Scripts
import           Ledger.Value

tname :: TokenName
tname = error ()

key :: PubKeyHash
key = error ()

-- BLOCK1
oneAtATimePolicy :: () -> ScriptContext -> Bool
oneAtATimePolicy _ ctx =
    -- 'ownCurrencySymbol' lets us get our own hash (= currency symbol)
    -- from the context
    let ownSymbol = ownCurrencySymbol ctx
        txinfo = scriptContextTxInfo ctx
        minted = txInfoMint txinfo
    -- Here we're looking at some specific token name, which we
    -- will assume we've got from elsewhere for now.
    in valueOf minted ownSymbol tname == 1

-- We can use 'compile' to turn a minting policy into a compiled Zerepoch Core program,
-- just as for validator scripts. We also provide a 'wrapMintingPolicy' function
-- to handle the boilerplate.
oneAtATimeCompiled :: CompiledCode (BuiltinData -> BuiltinData -> ())
oneAtATimeCompiled = $$(compile [|| wrapMintingPolicy oneAtATimePolicy ||])
-- BLOCK2
singleSignerPolicy :: ScriptContext -> Bool
singleSignerPolicy ctx = txSignedBy (scriptContextTxInfo ctx) key
-- BLOCK3
