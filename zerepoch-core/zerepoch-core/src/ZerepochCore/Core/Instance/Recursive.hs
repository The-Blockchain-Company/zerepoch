{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module ZerepochCore.Core.Instance.Recursive
    ( -- * Base functors
      TermF (..)
    , TypeF (..)
    , KindF (..)
    ) where

import           ZerepochCore.Core.Type
import           ZerepochPrelude

import           Data.Functor.Foldable.TH

$(join <$> traverse makeBaseFunctor [''Kind, ''Type, ''Term])
