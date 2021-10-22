-- | While the flexible pretty-printing infrastructure is useful when you want it,
-- it's helpful to have an implementation of the default Pretty typeclass that
-- does the default thing.

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZerepochCore.Core.Instance.Pretty.Default () where

import           ZerepochPrelude

import           ZerepochCore.Core.Instance.Pretty.Classic ()
import           ZerepochCore.Core.Type
import           ZerepochCore.Pretty.Classic
import           ZerepochCore.Pretty.PrettyConst

import           Universe

instance Pretty ann => Pretty (Kind ann) where
    pretty = prettyClassicDef

instance (PrettyClassic tyname, GShow uni, Pretty ann) => Pretty (Type tyname uni ann) where
    pretty = prettyClassicDef

instance
        ( PrettyClassic tyname
        , PrettyClassic name
        , GShow uni, Closed uni, uni `Everywhere` PrettyConst, Pretty fun
        , Pretty ann
        ) => Pretty (Term tyname name uni fun ann) where
    pretty = prettyClassicDef

instance
        ( PrettyClassic tyname
        , PrettyClassic name
        , GShow uni, Closed uni, uni `Everywhere` PrettyConst, Pretty fun
        , Pretty ann
        ) => Pretty (Program tyname name uni fun ann) where
    pretty = prettyClassicDef
