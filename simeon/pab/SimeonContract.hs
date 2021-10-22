{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}
module SimeonContract(SimeonContract(..), handlers) where

import           Control.Monad.Freer                 (interpret)
import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Default                        (Default (def))
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import qualified Language.Simeon.Client             as Simeon
import           Language.PureScript.Bridge          (equal, genericShow, mkSumType)
import           Zerepoch.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (contractHandler), HasDefinitions (..),
                                                      SomeBuiltin (..))
import qualified Zerepoch.PAB.Effects.Contract.Builtin as Builtin
import           Zerepoch.PAB.Run.PSGenerator          (HasPSTypes (psTypes))
import           Zerepoch.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Zerepoch.PAB.Simulator                as Simulator

data SimeonContract =
    SimeonApp -- The main simeon contract
    | WalletCompanion -- Wallet companion contract
    | SimeonFollower -- Follower contract
    deriving (Eq, Ord, Show, Read, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty SimeonContract where
    pretty = viaShow

instance HasDefinitions SimeonContract where
    getDefinitions = [ SimeonApp
                     , WalletCompanion
                     , SimeonFollower
                     ]
    getSchema = const [] -- TODO: replace with proper schemas using Builtin.endpointsToSchemas (missing some instances currently)
    getContract = \case
        SimeonApp      -> SomeBuiltin Simeon.simeonZerepochContract
        WalletCompanion -> SomeBuiltin Simeon.simeonCompanionContract
        SimeonFollower -> SomeBuiltin Simeon.simeonFollowContract

instance HasPSTypes SimeonContract where
    psTypes p = [ (equal <*> (genericShow <*> mkSumType)) p ]

handlers :: SimulatorEffectHandlers (Builtin SimeonContract)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler Builtin.handleBuiltin)
