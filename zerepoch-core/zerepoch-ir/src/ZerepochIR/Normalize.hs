{-# LANGUAGE FlexibleContexts #-}
-- | ZerepochIR versions of the functions in ZerepochCore.Normalize
module ZerepochIR.Normalize
    ( Export.normalizeType
    , normalizeTypesIn
    , normalizeTypesInProgram
    ) where

import           ZerepochCore.Core               as PLC (Normalized (..))
import           ZerepochCore.Name
import           ZerepochCore.Normalize          as Export (normalizeType)
import           ZerepochCore.Normalize.Internal hiding (normalizeTypesInM)
import           ZerepochCore.Quote
import           ZerepochCore.Rename             (rename)
import           ZerepochIR
import           ZerepochIR.Transform.Rename     ()

import           Control.Lens
import           Control.Monad                 ((>=>))
import           Universe                      (HasUniApply)

-- | Normalize every 'Type' in a 'Term'.
normalizeTypesIn
    :: (HasUnique tyname TypeUnique, HasUnique name TermUnique, MonadQuote m, HasUniApply uni)
    => Term tyname name uni fun ann -> m (Term tyname name uni fun ann)
normalizeTypesIn = rename >=> runNormalizeTypeM . normalizeTypesInM

-- | Normalize every 'Type' in a 'Program'.
normalizeTypesInProgram
    :: (HasUnique tyname TypeUnique, HasUnique name TermUnique, MonadQuote m, HasUniApply uni)
    => Program tyname name uni fun ann -> m (Program tyname name uni fun ann)
normalizeTypesInProgram (Program x t) = Program x <$> normalizeTypesIn t

-- | Normalize every 'Type' in a 'Term'.
-- Mirrors the `normalizeTypesInM` of 'ZerepochCore.Normalize.Internal', working on PIR.Term instead
normalizeTypesInM
    :: (HasUnique tyname TypeUnique, MonadQuote m, HasUniApply uni)
    => Term tyname name uni fun ann -> NormalizeTypeT m tyname uni ann (Term tyname name uni fun ann)
normalizeTypesInM = transformMOf termSubterms normalizeChildTypes where
    normalizeChildTypes = termSubtypes (fmap unNormalized . normalizeTypeM)
