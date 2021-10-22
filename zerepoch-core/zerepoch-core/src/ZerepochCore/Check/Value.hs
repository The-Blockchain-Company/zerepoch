{-# LANGUAGE OverloadedStrings #-}
module ZerepochCore.Check.Value
    ( isTermValue
    ) where

import           ZerepochCore.Core
import           ZerepochCore.Error

import           Data.Either

isTermValue :: Term tyname name uni fun ann -> Bool
isTermValue = isRight . termValue

termValue :: Term tyname name uni fun ann -> Either (NormCheckError tyname name uni fun ann) ()
termValue (IWrap _ _ _ term) = termValue term
termValue LamAbs {}          = pure ()
termValue TyAbs {}           = pure ()
termValue Constant {}        = pure ()
termValue t                  = Left $ BadTerm (termAnn t) t "term value"
