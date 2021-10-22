module UntypedZerepochCore.Check.Uniques
    ( checkProgram
    , checkTerm
    , UniqueError (..)
    , AsUniqueError (..)
    ) where

import           UntypedZerepochCore.Analysis.Definitions
import           UntypedZerepochCore.Core

import           ZerepochCore.Error
import           ZerepochCore.Name

import           Control.Monad.Error.Lens
import           Control.Monad.Except

import           Data.Foldable

checkProgram
    :: (Ord ann,
        HasUnique name TermUnique,
        AsUniqueError e ann,
        MonadError e m)
    => (UniqueError ann -> Bool)
    -> Program name uni fun ann
    -> m ()
checkProgram p (Program _ _ t) = checkTerm p t

checkTerm
    :: (Ord ann,
        HasUnique name TermUnique,
        AsUniqueError e ann,
        MonadError e m)
    => (UniqueError ann -> Bool)
    -> Term name uni fun ann
    -> m ()
checkTerm p t = do
    (_, errs) <- runTermDefs t
    for_ errs $ \e -> when (p e) $ throwing _UniqueError e
