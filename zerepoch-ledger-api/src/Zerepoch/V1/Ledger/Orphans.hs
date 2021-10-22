{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zerepoch.V1.Ledger.Orphans where

import           Data.Aeson        (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson        as JSON
import qualified Data.Aeson.Extras as JSON
import qualified Data.ByteString   as BSS
import qualified ZerepochTx.Builtins as ZerepochTx


instance ToJSON BSS.ByteString where
    toJSON = JSON.String . JSON.encodeByteString

instance FromJSON BSS.ByteString where
    parseJSON v = JSON.decodeByteString v

instance ToJSON ZerepochTx.BuiltinByteString where
    toJSON = JSON.String . JSON.encodeByteString . ZerepochTx.fromBuiltin

instance FromJSON ZerepochTx.BuiltinByteString where
    parseJSON v = ZerepochTx.toBuiltin <$> JSON.decodeByteString v
