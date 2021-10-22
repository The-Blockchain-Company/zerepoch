-- Need some extra imports from the Prelude for doctests, annoyingly
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fmax-simplifier-iterations=0 #-}

module ZerepochTx.Prelude (
    -- $prelude
    -- * Classes
    module Eq,
    module Enum,
    module Ord,
    module Semigroup,
    module Monoid,
    module Numeric,
    module Functor,
    module Applicative,
    module Lattice,
    module Foldable,
    module Traversable,
    -- * Monad
    (>>=),
    (=<<),
    (>>),
    return,
    -- * Standard functions
    ($),
    (.),
    otherwise,
    until,
    flip,
    -- * Tracing functions
    module Trace,
    -- * String
    BuiltinString,
    appendString,
    emptyString,
    equalsString,
    encodeUtf8,
    -- * Error
    error,
    check,
    -- * Booleans
    module Bool,
    -- * Integer numbers
    Integer,
    divide,
    modulo,
    quotient,
    remainder,
    even,
    -- * Tuples
    fst,
    snd,
    curry,
    uncurry,
    -- * Maybe
    module Maybe,
    -- * Either
    module Either,
    -- * Lists
    module List,
    dropWhile,
    zipWith,
    -- * ByteStrings
    BuiltinByteString,
    appendByteString,
    consByteString,
    takeByteString,
    dropByteString,
    sliceByteString,
    lengthOfByteString,
    indexByteString,
    emptyByteString,
    decodeUtf8,
    -- * Hashes and Signatures
    sha2_256,
    sha3_256,
    verifySignature,
    -- * Rational numbers
    Rational,
    (%),
    fromInteger,
    round,
    divMod,
    quotRem,
    -- * Data
    BuiltinData,
    fromBuiltin,
    toBuiltin
    ) where

import           Data.String          (IsString (..))
import           ZerepochCore.Data      (Data (..))
import           ZerepochTx.Applicative as Applicative
import           ZerepochTx.Bool        as Bool
import           ZerepochTx.Builtins    (BuiltinByteString, BuiltinData, BuiltinString, appendByteString, appendString,
                                       consByteString, decodeUtf8, emptyByteString, emptyString, encodeUtf8,
                                       equalsByteString, equalsString, error, fromBuiltin, greaterThanByteString,
                                       indexByteString, lengthOfByteString, lessThanByteString, sha2_256, sha3_256,
                                       sliceByteString, toBuiltin, trace, verifySignature)
import qualified ZerepochTx.Builtins    as Builtins
import           ZerepochTx.Either      as Either
import           ZerepochTx.Enum        as Enum
import           ZerepochTx.Eq          as Eq
import           ZerepochTx.Foldable    as Foldable
import           ZerepochTx.Functor     as Functor
import           ZerepochTx.IsData
import           ZerepochTx.Lattice     as Lattice
import           ZerepochTx.List        as List hiding (foldr)
import           ZerepochTx.Maybe       as Maybe
import           ZerepochTx.Monoid      as Monoid
import           ZerepochTx.Numeric     as Numeric
import           ZerepochTx.Ord         as Ord
import           ZerepochTx.Ratio       as Ratio
import           ZerepochTx.Semigroup   as Semigroup
import           ZerepochTx.Trace       as Trace
import           ZerepochTx.Traversable as Traversable
import           Prelude              hiding (Applicative (..), Enum (..), Eq (..), Foldable (..), Functor (..),
                                       Monoid (..), Num (..), Ord (..), Rational, Semigroup (..), Traversable (..), all,
                                       and, any, concat, concatMap, const, curry, divMod, either, elem, error, filter,
                                       fst, head, id, length, map, mapM_, max, maybe, min, not, notElem, null, or,
                                       quotRem, reverse, round, sequence, snd, take, uncurry, zip, (!!), ($), (&&),
                                       (++), (<$>), (||))

-- this module does lots of weird stuff deliberately
{- HLINT ignore -}

-- $prelude
-- The ZerepochTx Prelude is a replacement for the Haskell Prelude that works
-- better with Zerepoch Tx. You should use it if you're writing code that
-- will be compiled with the Zerepoch Tx compiler.
-- @
--     {-# LANGUAGE NoImplicitPrelude #-}
--     import ZerepochTx.Prelude
-- @

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> import ZerepochTx.Prelude

{-# INLINABLE check #-}
-- | Checks a 'Bool' and aborts if it is false.
check :: Bool -> ()
check b = if b then () else traceError "Pd" {-"Check has failed"-}

{-# INLINABLE divide #-}
-- | Integer division, rounding downwards
--
--   >>> divide (-41) 5
--   -9
--
divide :: Integer -> Integer -> Integer
divide = Builtins.divideInteger

{-# INLINABLE modulo #-}
-- | Integer remainder, always positive for a positive divisor
--
--   >>> modulo (-41) 5
--   4
--
modulo :: Integer -> Integer -> Integer
modulo = Builtins.modInteger

{-# INLINABLE quotient #-}
-- | Integer division, rouding towards zero
--
--   >>> quotient (-41) 5
--   -8
--

quotient :: Integer -> Integer -> Integer
quotient = Builtins.quotientInteger

{-# INLINABLE remainder #-}
-- | Integer remainder, same sign as dividend
--
--   >>> remainder (-41) 5
--   -1
--
remainder :: Integer -> Integer -> Integer
remainder = Builtins.remainderInteger

{-# INLINABLE fst #-}
-- | Zerepoch Tx version of 'Data.Tuple.fst'
fst :: (a, b) -> a
fst (a, _) = a

{-# INLINABLE snd #-}
-- | Zerepoch Tx version of 'Data.Tuple.snd'
snd :: (a, b) -> b
snd (_, b) = b

{-# INLINABLE curry #-}
curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

{-# INLINABLE uncurry #-}
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

infixr 0 $
-- Normal $ is levity-polymorphic, which we can't handle.
{-# INLINABLE ($) #-}
-- | Zerepoch Tx version of 'Data.Function.($)'.
($) :: (a -> b) -> a -> b
f $ a = f a

{-# INLINABLE takeByteString #-}
-- | Returns the n length prefix of a 'ByteString'.
takeByteString :: Integer -> BuiltinByteString -> BuiltinByteString
takeByteString n bs = Builtins.sliceByteString 0 (toBuiltin n) bs

{-# INLINABLE dropByteString #-}
-- | Returns the suffix of a 'ByteString' after n elements.
dropByteString :: Integer -> BuiltinByteString -> BuiltinByteString
dropByteString n bs = Builtins.sliceByteString (toBuiltin n) (Builtins.lengthOfByteString bs - n) bs
