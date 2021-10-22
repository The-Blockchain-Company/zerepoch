-- | Functions related to @integer@.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module ZerepochCore.StdLib.Data.Integer
    ( integer
    , succInteger
    ) where

import           ZerepochCore.Core
import           ZerepochCore.Default.Builtins
import           ZerepochCore.MkPlc
import           ZerepochCore.Name
import           ZerepochCore.Quote

import           Universe

integer :: uni `Includes` Integer => Type tyname uni ()
integer = mkTyBuiltin @_ @Integer ()

-- |  @succ :: Integer -> Integer@ as a PLC term.
--
-- > \(i : integer) -> addInteger i 1
succInteger :: (TermLike term TyName Name uni DefaultFun, uni `Includes` Integer) => term ()
succInteger = runQuote $ do
    i  <- freshName "i"
    return
        . lamAbs () i integer
        . mkIterApp () (builtin () AddInteger)
        $ [ var () i
          , mkConstant @Integer () 1
          ]
