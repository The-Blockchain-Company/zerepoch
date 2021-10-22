module Ledger.Contexts
    ( module Export
    , pubKeyHash
    , scriptCurrencySymbol
    ) where

import           Ledger.Crypto             (pubKeyHash)
import           Ledger.Scripts            (MintingPolicy, MintingPolicyHash (..), mintingPolicyHash)
import           Zerepoch.V1.Ledger.Contexts as Export
import           Zerepoch.V1.Ledger.Value    (CurrencySymbol (..))
import qualified Zerepoch.V1.Ledger.Value    as Value

{-# INLINABLE scriptCurrencySymbol #-}
-- | The 'CurrencySymbol' of a 'MintingPolicy'
scriptCurrencySymbol :: MintingPolicy -> CurrencySymbol
scriptCurrencySymbol scrpt = let (MintingPolicyHash hsh) = mintingPolicyHash scrpt in Value.CurrencySymbol hsh

