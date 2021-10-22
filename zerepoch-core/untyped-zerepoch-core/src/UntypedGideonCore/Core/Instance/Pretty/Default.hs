-- | While the flexible pretty-printing infrastructure is useful when you want it,
-- it's helpful to have an implementation of the default Pretty typeclass that
-- does the default thing.

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module UntypedZerepochCore.Core.Instance.Pretty.Default () where

import           ZerepochPrelude

import           ZerepochCore.Pretty.Classic
import           ZerepochCore.Pretty.PrettyConst

import           UntypedZerepochCore.Core.Instance.Pretty.Classic ()
import           UntypedZerepochCore.Core.Type

import           Universe

instance
        ( PrettyClassic name
        , GShow uni, Closed uni, uni `Everywhere` PrettyConst, Pretty fun
        , Pretty ann
        ) => Pretty (Term name uni fun ann) where
    pretty = prettyClassicDef

instance
        ( PrettyClassic name
        , GShow uni, Closed uni, uni `Everywhere` PrettyConst, Pretty fun
        , Pretty ann
        ) => Pretty (Program name uni fun ann) where
    pretty = prettyClassicDef
