{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-strictness   #-}

-- | The type of transaction IDs
module Zerepoch.V1.Ledger.TxId(
    TxId (..)
    ) where

import           Codec.Serialise.Class     (Serialise)
import           Control.DeepSeq           (NFData)
import           Data.Aeson                (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           Data.String               (IsString)
import           Data.Text.Prettyprint.Doc (Pretty)
import           GHC.Generics              (Generic)
import           Zerepoch.V1.Ledger.Bytes    (LedgerBytes (..))
import           Zerepoch.V1.Ledger.Orphans  ()
import qualified ZerepochTx                  as ZerepochTx
import qualified ZerepochTx.Prelude          as ZerepochTx

-- | A transaction ID, using a SHA256 hash as the transaction id.
newtype TxId = TxId { getTxId :: ZerepochTx.BuiltinByteString }
    deriving (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, NFData)
    deriving newtype (ZerepochTx.Eq, ZerepochTx.Ord, Serialise)
    deriving (Show, Pretty, IsString) via LedgerBytes

ZerepochTx.makeLift ''TxId
ZerepochTx.makeIsDataIndexed ''TxId [('TxId,0)]
