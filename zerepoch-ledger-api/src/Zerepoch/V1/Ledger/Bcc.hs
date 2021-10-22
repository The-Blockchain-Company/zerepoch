{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
-- Otherwise we get a complaint about the 'fromIntegral' call in the generated instance of 'Integral' for 'Bcc'
{-# OPTIONS_GHC -Wno-identities #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
-- | Functions for working with 'Bcc' in Template Haskell.
module Zerepoch.V1.Ledger.Bcc(
      Bcc (..)
    , getBcc
    , bccSymbol
    , bccToken
    -- * Constructors
    , fromValue
    , toValue
    , entropicOf
    , bccOf
    , entropicValueOf
    , bccValueOf
    -- * Num operations
    , divide
    -- * Etc.
    , isZero
    ) where

import qualified Prelude                          as Haskell

import           Data.Fixed

import           Codec.Serialise.Class            (Serialise)
import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Tagged
import           Data.Text.Prettyprint.Doc.Extras
import           GHC.Generics                     (Generic)
import           Zerepoch.V1.Ledger.Value           (CurrencySymbol (..), TokenName (..), Value)
import qualified Zerepoch.V1.Ledger.Value           as TH
import qualified ZerepochTx                         as ZerepochTx
import           ZerepochTx.Lift                    (makeLift)
import           ZerepochTx.Prelude                 hiding (divide)
import qualified ZerepochTx.Prelude                 as P

{-# INLINABLE bccSymbol #-}
-- | The 'CurrencySymbol' of the 'Bcc' currency.
bccSymbol :: CurrencySymbol
bccSymbol = CurrencySymbol emptyByteString

{-# INLINABLE bccToken #-}
-- | The 'TokenName' of the 'Bcc' currency.
bccToken :: TokenName
bccToken = TokenName emptyByteString

-- | BCC, the special currency on the Bcc blockchain. The unit of Bcc is Entropic, and
--   1M Entropic is one Bcc.
--   See note [Currencies] in 'Ledger.Validation.Value.TH'.
newtype Bcc = Entropic { getEntropic :: Integer }
    deriving (Haskell.Enum)
    deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving newtype (Eq, Ord, Haskell.Num, AdditiveSemigroup, AdditiveMonoid, AdditiveGroup, MultiplicativeSemigroup, MultiplicativeMonoid, Haskell.Integral, Haskell.Real, Serialise, ZerepochTx.ToData, ZerepochTx.FromData, ZerepochTx.UnsafeFromData)
    deriving Pretty via (Tagged "Entropic:" Integer)

instance Haskell.Semigroup Bcc where
    Entropic a1 <> Entropic a2 = Entropic (a1 + a2)

instance Semigroup Bcc where
    Entropic a1 <> Entropic a2 = Entropic (a1 + a2)

instance Haskell.Monoid Bcc where
    mempty = Entropic 0

instance Monoid Bcc where
    mempty = Entropic 0

makeLift ''Bcc

{-# INLINABLE getBcc #-}
-- | Get the amount of Bcc (the unit of the currency Bcc) in this 'Bcc' value.
getBcc :: Bcc -> Micro
getBcc (Entropic i) = MkFixed i

{-# INLINABLE toValue #-}
-- | Create a 'Value' containing only the given 'Bcc'.
toValue :: Bcc -> Value
toValue (Entropic i) = TH.singleton bccSymbol bccToken i

{-# INLINABLE fromValue #-}
-- | Get the 'Bcc' in the given 'Value'.
fromValue :: Value -> Bcc
fromValue v = Entropic (TH.valueOf v bccSymbol bccToken)

{-# INLINABLE entropicOf #-}
-- | Create 'Bcc' representing the given quantity of Entropic (the unit of the currency Bcc).
entropicOf :: Integer -> Bcc
entropicOf = Entropic

{-# INLINABLE bccOf #-}
-- | Create 'Bcc' representing the given quantity of Bcc (1M Entropic).
bccOf :: Micro -> Bcc
bccOf (MkFixed x) = Entropic x

{-# INLINABLE entropicValueOf #-}
-- | A 'Value' with the given amount of Entropic (the currency unit).
--
--   @entropicValueOf == toValue . entropicOf@
--
entropicValueOf :: Integer -> Value
entropicValueOf = TH.singleton bccSymbol bccToken

{-# INLINABLE bccValueOf #-}
-- | A 'Value' with the given amount of Bcc (the currency unit).
--
--   @bccValueOf == toValue . bccOf@
--
bccValueOf :: Micro -> Value
bccValueOf (MkFixed x) = TH.singleton bccSymbol bccToken x

{-# INLINABLE divide #-}
-- | Divide one 'Bcc' value by another.
divide :: Bcc -> Bcc -> Bcc
divide (Entropic a) (Entropic b) = Entropic (P.divide a b)

{-# INLINABLE isZero #-}
-- | Check whether an 'Bcc' value is zero.
isZero :: Bcc -> Bool
isZero (Entropic i) = i == 0
