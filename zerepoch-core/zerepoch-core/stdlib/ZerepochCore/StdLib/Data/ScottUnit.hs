-- | Scott-encoded @unit@ and related functions.

{-# LANGUAGE OverloadedStrings #-}

module ZerepochCore.StdLib.Data.ScottUnit
    ( unit
    , unitval
    ) where

import           ZerepochCore.Core
import           ZerepochCore.MkPlc
import           ZerepochCore.Name
import           ZerepochCore.Quote

-- | '()' as a PLC type.
--
-- > all (A :: *). A -> A
unit :: Type TyName uni ()
unit = runQuote $ do
    a <- freshTyName "a"
    return
        . TyForall () a (Type ())
        . TyFun () (TyVar () a)
        $ TyVar () a

-- | '()' as a PLC term.
--
-- > /\(A :: *) -> \(x : A) -> x
unitval :: TermLike term TyName Name uni fun => term ()
unitval = runQuote $ do
    a <- freshTyName "a"
    x <- freshName "x"
    return
        . tyAbs  () a (Type ())
        . lamAbs () x (TyVar () a)
        $ var () x
