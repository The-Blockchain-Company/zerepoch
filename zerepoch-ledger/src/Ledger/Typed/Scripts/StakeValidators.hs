{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
module Ledger.Typed.Scripts.StakeValidators (
    WrappedStakeValidatorType
    , wrapStakeValidator
    , mkForwardingStakeValidator
    , forwardToValidator
    , Any
    ) where

import           ZerepochTx
import           ZerepochTx.Prelude

import           Zerepoch.V1.Ledger.Address    (Address (..))
import           Zerepoch.V1.Ledger.Contexts   (ScriptContext (..), ScriptPurpose (..), TxInfo (..))
import qualified Zerepoch.V1.Ledger.Contexts   as Validation
import           Zerepoch.V1.Ledger.Credential (Credential (..))
import           Zerepoch.V1.Ledger.Scripts
import           Zerepoch.V1.Ledger.Tx         (TxOut (..))

import           Ledger.Typed.TypeUtils

type WrappedStakeValidatorType = BuiltinData -> BuiltinData -> ()

-- TODO: we should add a TypedStakeValidator interface here

{-# INLINABLE wrapStakeValidator #-}
wrapStakeValidator
    :: UnsafeFromData r
    => (r -> Validation.ScriptContext -> Bool)
    -> WrappedStakeValidatorType
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
wrapStakeValidator f r p = check $ f (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)

-- | A stake validator that checks whether the validator script was run
--   in the right transaction.
mkForwardingStakeValidator :: ValidatorHash -> StakeValidator
mkForwardingStakeValidator vshsh =
    mkStakeValidatorScript
    $ $$(ZerepochTx.compile [|| \(hsh :: ValidatorHash) -> wrapStakeValidator (forwardToValidator hsh) ||])
       `ZerepochTx.applyCode` ZerepochTx.liftCode vshsh

{-# INLINABLE forwardToValidator #-}
forwardToValidator :: ValidatorHash -> () -> ScriptContext -> Bool
forwardToValidator h _ ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}, scriptContextPurpose} =
    let checkHash TxOut{txOutAddress=Address{addressCredential=ScriptCredential vh}} = vh == h
        checkHash _                                                                  = False
        result = any (checkHash . Validation.txInInfoResolved) txInfoInputs
    in case scriptContextPurpose of
        Rewarding _  -> result
        Certifying _ -> result
        _            -> False
