module Ledger.Address
    ( module Export
    , pubKeyAddress
    , scriptAddress
    ) where

import           Ledger.Crypto               (pubKeyHash)
import           Ledger.Scripts              (Validator, validatorHash)
import           Zerepoch.V1.Ledger.Address    as Export
import           Zerepoch.V1.Ledger.Credential (Credential (..))
import           Zerepoch.V1.Ledger.Crypto     (PubKey)

{-# INLINABLE pubKeyAddress #-}
-- | The address that should be targeted by a transaction output locked by the given public key.
pubKeyAddress :: PubKey -> Address
pubKeyAddress pk = Address (PubKeyCredential (pubKeyHash pk)) Nothing

-- | The address that should be used by a transaction output locked by the given validator script.
scriptAddress :: Validator -> Address
scriptAddress validator = Address (ScriptCredential (validatorHash validator)) Nothing
