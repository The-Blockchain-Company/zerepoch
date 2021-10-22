module UntypedZerepochCore
    ( module Export
    , applyProgram
    , parseScoped
    , PLC.DefaultUni
    , PLC.DefaultFun
    ) where

import           UntypedZerepochCore.Check.Uniques      as Uniques
import           UntypedZerepochCore.Parser             as Parser
import           UntypedZerepochCore.Rename             as Rename

import           ZerepochCore.Name                      as Export
import           UntypedZerepochCore.Core               as Export
import           UntypedZerepochCore.Core.Instance.Flat as Export
import           UntypedZerepochCore.DeBruijn           as Export
import           UntypedZerepochCore.Size               as Export
import           UntypedZerepochCore.Subst              as Export
import           UntypedZerepochCore.Transform.Simplify as Export
-- Also has some functions


import qualified ZerepochCore                           as PLC
import qualified ZerepochCore.Error                     as PLC
import           ZerepochPrelude                        (through)

import           Control.Monad.Except                 (MonadError, (<=<))
import qualified Data.ByteString.Lazy                 as BSL


-- | Take one UPLC program and apply it to another.
applyProgram :: Program name uni fun () -> Program name uni fun () -> Program name uni fun ()
applyProgram (Program _ _ t1) (Program _ _ t2) = Program () (PLC.defaultVersion ()) (Apply () t1 t2)

-- | Parse and rewrite so that names are globally unique, not just unique within
-- their scope.
parseScoped
    :: (PLC.AsParseError e PLC.AlexPosn,
        PLC.AsUniqueError e PLC.AlexPosn,
        MonadError e m,
        PLC.MonadQuote m)
    => BSL.ByteString
    -> m (Program PLC.Name PLC.DefaultUni PLC.DefaultFun PLC.AlexPosn)
-- don't require there to be no free variables at this point, we might be parsing an open term
parseScoped = through (Uniques.checkProgram (const True)) <=< Rename.rename <=< Parser.parseProgram
