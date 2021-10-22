module UntypedZerepochCore.Mark
    ( markNonFreshTerm
    , markNonFreshProgram
    ) where

import           ZerepochCore.Core         (HasUniques)
import           ZerepochCore.Name
import           ZerepochCore.Quote
import           UntypedZerepochCore.Core
import           UntypedZerepochCore.Subst

-- | Marks all the 'Unique's in a term as used, so they will not be generated in future. Useful if you
-- have a term which was not generated in 'Quote'.
markNonFreshTerm
    :: (HasUniques (Term name uni fun ann), MonadQuote m)
    => Term name uni fun ann -> m ()
markNonFreshTerm = markNonFreshMax . uniquesTerm

-- | Marks all the 'Unique's in a program as used, so they will not be generated in future. Useful if you
-- have a program which was not generated in 'Quote'.
markNonFreshProgram
    :: (HasUnique name TermUnique, MonadQuote m)
    => Program name uni fun ann
    -> m ()
markNonFreshProgram (Program _ _ body) = markNonFreshTerm body
