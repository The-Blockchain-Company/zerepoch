{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module ZerepochCore.Examples.Data.List
    ( omapList
    ) where

import           ZerepochCore.Core
import           ZerepochCore.Default
import           ZerepochCore.MkPlc
import           ZerepochCore.Name
import           ZerepochCore.Quote

import           ZerepochCore.StdLib.Data.Function
import           ZerepochCore.StdLib.Data.List

import           ZerepochCore.Examples.Builtins

-- | Monomorphic @map@ over built-in lists.
--
-- /\(a :: *) -> \(f : a -> a) ->
--     fix {list a} {list a} \(rec : list a -> list a) (xs : list a) ->
--         caseList {a} xs {list a} xs \(x : a) (xs' : list a) -> cons {a} (f x) (rec xs')
omapList :: Term TyName Name DefaultUni (Either DefaultFun ExtensionFun) ()
omapList = runQuote $ do
    a   <- freshTyName "a"
    f   <- freshName "f"
    rec <- freshName "rec"
    xs  <- freshName "xs"
    x   <- freshName "x"
    xs' <- freshName "xs'"
    let listA = TyApp () list $ TyVar () a
        unwrap' ann = apply ann . tyInst () (mapFun Left caseList) $ TyVar () a
    return
        . tyAbs () a (Type ())
        . lamAbs () f (TyFun () (TyVar () a) $ TyVar () a)
        . apply () (mkIterInst () fix [listA, listA])
        . lamAbs () rec (TyFun () listA listA)
        . lamAbs () xs listA
        . apply () (apply () (tyInst () (unwrap' () (var () xs)) listA) $ var () xs)
        . lamAbs () x (TyVar () a)
        . lamAbs () xs' listA
        $ mkIterApp () (tyInst () (builtin () $ Right Cons) $ TyVar () a)
            [ apply () (var () f) $ var () x
            , apply () (var () rec) $ var () xs'
            ]
