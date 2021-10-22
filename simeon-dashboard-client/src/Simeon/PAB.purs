module Simeon.PAB
  ( transactionFee
  , contractCreationFee
  , ZerepochAppId(..)
  ) where

import Prelude
import Data.BigInteger (BigInteger, fromInt)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)

-- In the Simeon PAB, transactions have a fixed cost of 10 entropic; in the real node, transaction
-- fees will vary, but this will serve as an approximation for now.
transactionFee :: BigInteger
transactionFee = fromInt 10

-- FIXME: it appears that creating a contract currently requires three transactions, but I believe it
-- should be possible with one; check this with Alex
contractCreationFee :: BigInteger
contractCreationFee = transactionFee * (fromInt 3)

{-
A `ZerepochAppId` is used to identify an instance of a Zerepoch "contract" in the PAB. In the PAB code it is
called `ContractInstanceId` - as above, we don't refer to "contracts" here so as to avoid confusion with
*Simeon* contracts. This is converted to a `ContractInstanceId` that the PAB understands by the `Bridge`
module.
-}
newtype ZerepochAppId
  = ZerepochAppId UUID

derive instance newtypeZerepochAppId :: Newtype ZerepochAppId _

derive instance eqZerepochAppId :: Eq ZerepochAppId

derive instance ordZerepochAppId :: Ord ZerepochAppId

derive instance genericZerepochAppId :: Generic ZerepochAppId _

-- note we need to encode this type, not to communicate with the PAB (we have the `ContractInstanceId`
-- for that), but to save `WalletData` to local storage
instance encodeZerepochAppId :: Encode ZerepochAppId where
  encode value = genericEncode defaultOptions value

instance decodeZerepochAppId :: Decode ZerepochAppId where
  decode value = genericDecode defaultOptions value
