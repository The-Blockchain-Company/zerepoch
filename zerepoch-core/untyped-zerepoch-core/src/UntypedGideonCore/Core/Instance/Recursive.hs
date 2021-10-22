{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module UntypedZerepochCore.Core.Instance.Recursive
    ( -- * Base functors
      TermF (..)
    ) where

import           UntypedZerepochCore.Core.Type

import           Data.Functor.Foldable.TH

$(makeBaseFunctor ''Term)
