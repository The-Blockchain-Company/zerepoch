{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-

Interface to beam ecosystem used by the PAB to store contracts.

-}
module Zerepoch.PAB.Db.Beam
  where

import           Bcc.BM.Trace                    (Trace)
import           Control.Monad.Freer                 (Eff, interpret, reinterpret, runM, subsume)
import           Control.Monad.Freer.Delay           (DelayEffect, handleDelayEffect)
import           Control.Monad.Freer.Error           (runError)
import           Control.Monad.Freer.Extras          (LogMsg, mapLog)
import qualified Control.Monad.Freer.Extras.Modify   as Modify
import           Control.Monad.Freer.Reader          (runReader)
import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Typeable                       (Typeable)
import           Database.SQLite.Simple              (Connection)
import           Zerepoch.PAB.Db.Beam.ContractStore    (handleContractStore)
import           Zerepoch.PAB.Effects.Contract         (ContractStore)
import           Zerepoch.PAB.Effects.Contract.Builtin (Builtin, HasDefinitions)
import           Zerepoch.PAB.Effects.DbStore
import           Zerepoch.PAB.Monitoring.Monitoring    (handleLogMsgTrace)
import           Zerepoch.PAB.Monitoring.PABLogMsg     (PABLogMsg (..), PABMultiAgentMsg (..))
import           Zerepoch.PAB.Types                    (PABError)


-- | Run the ContractStore and ContractDefinitionStore effects on the
--   SQLite database.
runBeamStoreAction ::
    forall a b.
    ( ToJSON a
    , FromJSON a
    , HasDefinitions a
    , Typeable a
    )
    => Connection
    -> Trace IO (PABLogMsg (Builtin a))
    -> Eff '[ContractStore (Builtin a), LogMsg (PABMultiAgentMsg (Builtin a)), DelayEffect, IO] b
    -> IO (Either PABError b)
runBeamStoreAction connection trace =
    runM
    . runError
    . runReader connection
    . interpret (handleDbStore trace)
    . subsume @IO
    . handleDelayEffect
    . interpret (handleLogMsgTrace trace)
    . reinterpret (mapLog SMultiAgent)
    . interpret handleContractStore
    . Modify.raiseEnd
