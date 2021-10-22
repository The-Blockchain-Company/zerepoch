{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module ZerepochCore.Examples.Data.Pair
    ( obothPair
    ) where

import           ZerepochCore.Core
import           ZerepochCore.Default
import           ZerepochCore.MkPlc
import           ZerepochCore.Name
import           ZerepochCore.Quote

import           ZerepochCore.StdLib.Data.Pair

import           ZerepochCore.Examples.Builtins

-- | Apply a monomorphic function to both components of a pair.
--
-- > /\(a :: *) -> \(f : a -> a) (p : pair a a) ->
-- >     comma {a} {a} (f (fst {a} {a} p)) (f (snd {a} {a} p))
obothPair :: TermLike term TyName Name DefaultUni (Either DefaultFun ExtensionFun) => term ()
obothPair = runQuote $ do
    a <- freshTyName "a"
    f <- freshName "f"
    p <- freshName "p"
    let atAA fun = mkIterInst () (builtin () fun) [TyVar () a, TyVar () a]
    return
        . tyAbs () a (Type ())
        . lamAbs () f (TyFun () (TyVar () a) $ TyVar () a)
        . lamAbs () p (mkIterTyApp () pair [TyVar () a, TyVar () a])
        $ mkIterApp () (atAA $ Right Comma)
            [ apply () (var () f) . apply () (atAA $ Left FstPair) $ var () p
            , apply () (var () f) . apply () (atAA $ Left SndPair) $ var () p
            ]
