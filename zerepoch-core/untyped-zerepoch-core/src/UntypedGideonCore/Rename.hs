-- | The user-facing API of the untyped renamer.
-- See ZerepochCore.Rename for details.

{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UntypedZerepochCore.Rename
    ( Rename (..)
    ) where

import           ZerepochPrelude

import           UntypedZerepochCore.Core
import           UntypedZerepochCore.Mark
import           UntypedZerepochCore.Rename.Internal

import           ZerepochCore.Core                   (HasUniques)
import           ZerepochCore.Name
import           ZerepochCore.Rename                 (Rename (..))

instance HasUniques (Term name uni fun ann) => Rename (Term name uni fun ann) where
    -- See Note [Marking].
    rename = through markNonFreshTerm >=> runRenameT . renameTermM

instance HasUniques (Program name uni fun ann) => Rename (Program name uni fun ann) where
    -- See Note [Marking].
    rename = through markNonFreshProgram >=> runRenameT . renameProgramM
