{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Configuring and calculating transaction fees in the emulator.
module Ledger.Fee(
  FeeConfig (..)
, calcFees
) where

import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Default         (Default (def))
import           GHC.Generics         (Generic)
import           Ledger.Index         (minFee)
import           Zerepoch.V1.Ledger.Bcc (Bcc)
import qualified Zerepoch.V1.Ledger.Bcc as Bcc

-- | Datatype to configure the fee in a transaction.
--
-- The fee for a transaction is typically: 'fcConstantFee + 'fcScriptsFeeFactor' *
-- <SIZE_DEPENDANT_SCRIPTS_FEE>.
data FeeConfig =
    FeeConfig
        { fcConstantFee      :: Bcc    -- ^ Constant fee per transaction in entropic
        , fcScriptsFeeFactor :: Double -- ^ Factor by which to multiply the size-dependent scripts fee
        }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Default FeeConfig where
  def = FeeConfig { fcConstantFee = Bcc.fromValue $ minFee mempty
                  , fcScriptsFeeFactor = 1.0
                  }

calcFees :: FeeConfig
         -> Integer -- ^ Scripts fee in entropic
         -> Bcc -- ^ Fees in entropic
calcFees FeeConfig { fcConstantFee , fcScriptsFeeFactor } scriptsFee =
     fcConstantFee
  <> Bcc.entropicOf (floor $ fcScriptsFeeFactor * fromIntegral scriptsFee)
