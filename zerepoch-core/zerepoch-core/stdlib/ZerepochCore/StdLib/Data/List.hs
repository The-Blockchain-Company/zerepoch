-- | Built-in @list@ and related functions.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module ZerepochCore.StdLib.Data.List
    ( list
    , caseList
    , foldrList
    ) where

import           ZerepochCore.Core
import           ZerepochCore.Default
import           ZerepochCore.MkPlc
import           ZerepochCore.Name
import           ZerepochCore.Quote

import           ZerepochCore.StdLib.Data.Bool
import           ZerepochCore.StdLib.Data.Function
import           ZerepochCore.StdLib.Data.Unit

-- | @[]@ as a built-in PLC type.
list :: uni `Contains` [] => Type TyName uni ()
list = mkTyBuiltin @_ @[] ()

-- See Note [Pattern matching on built-in types].
-- | Pattern matching on built-in lists. @caseList {a} xs@ on built-in lists is
-- equivalent to @unwrap xs@ on lists defined in PLC itself (hence why we bind @r@ after @xs@).
--
-- > /\(a :: *) -> \(xs : list a) -> /\(r :: *) -> (z : r) (f : a -> list a -> r) ->
-- >     ifThenElse
-- >         {r}
-- >         (Null {a} xs)
-- >         (\(u : ()) -> z)
-- >         (\(u : ()) -> f (head {a} xs) (tail {a} xs))
caseList :: TermLike term TyName Name DefaultUni DefaultFun => term ()
caseList = runQuote $ do
    a <- freshTyName "a"
    r <- freshTyName "r"
    xs <- freshName "x"
    z <- freshName "z"
    f <- freshName "f"
    u <- freshName "u"
    let listA = TyApp () list $ TyVar () a
        funAtXs fun = apply () (tyInst () (builtin () fun) $ TyVar () a) $ var () xs
    return
        . tyAbs () a (Type ())
        . lamAbs () xs listA
        . tyAbs () r (Type ())
        . lamAbs () z (TyVar () r)
        . lamAbs () f (TyFun () (TyVar () a) . TyFun () listA $ TyVar () r)
        $ mkIterApp () (tyInst () ifThenElse $ TyVar () r)
            [ funAtXs NullList
            , lamAbs () u unit $ var () z
            , lamAbs () u unit $ mkIterApp () (var () f) [funAtXs HeadList, funAtXs TailList]
            ]

-- |  @foldr@ over built-in lists.
--
-- > /\(a :: *) (r :: *) -> \(f : a -> r -> r) (z : r) ->
-- >     fix {list a} {r} \(rec : list a -> r) (xs : list a) ->
-- >         caseList {a} xs {r} z \(x : a) (xs' : list a) -> f x (rec xs')
foldrList :: TermLike term TyName Name DefaultUni DefaultFun => term ()
foldrList = runQuote $ do
    a   <- freshTyName "a"
    r   <- freshTyName "r"
    f   <- freshName "f"
    z   <- freshName "z"
    rec <- freshName "rec"
    xs  <- freshName "xs"
    x   <- freshName "x"
    xs' <- freshName "xs'"
    let listA = TyApp () list $ TyVar () a
        unwrap' ann = apply ann . tyInst () caseList $ TyVar () a
    -- Copypasted verbatim from @foldrList@ over Scott-encoded lists.
    return
        . tyAbs () a (Type ())
        . tyAbs () r (Type ())
        . lamAbs () f (TyFun () (TyVar () a) . TyFun () (TyVar () r) $ TyVar () r)
        . lamAbs () z (TyVar () r)
        . apply () (mkIterInst () fix [listA, TyVar () r])
        . lamAbs () rec (TyFun () listA $ TyVar () r)
        . lamAbs () xs listA
        . apply () (apply () (tyInst () (unwrap' () (var () xs)) $ TyVar () r) $ var () z)
        . lamAbs () x (TyVar () a)
        . lamAbs () xs' listA
        $ mkIterApp () (var () f)
            [ var () x
            , apply () (var () rec) $ var () xs'
            ]
