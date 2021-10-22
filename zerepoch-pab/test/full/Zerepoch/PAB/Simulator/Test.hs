{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-

A 'Simulator' for the test contracts

-}
module Zerepoch.PAB.Simulator.Test(runSimulation) where

import           Control.Monad.Freer                      (interpret)
import           Data.Default                             (Default (def))
import           Zerepoch.PAB.Core                          (EffectHandlers)
import           Zerepoch.PAB.Effects.Contract.Builtin      (Builtin, BuiltinHandler (contractHandler), handleBuiltin)
import           Zerepoch.PAB.Effects.Contract.ContractTest (TestContracts (..))
import           Zerepoch.PAB.Simulator                     (Simulation, SimulatorContractHandler, SimulatorState,
                                                           mkSimulatorHandlers, runSimulationWith)
import           Zerepoch.PAB.Types                         (PABError)

-- | Run the PAB simulator with the test contracts
runSimulation :: Simulation (Builtin TestContracts) a -> IO (Either PABError a)
runSimulation = runSimulationWith simulatorHandlers

-- | 'EffectHandlers' for running the PAB as a simulator (no connectivity to
--   out-of-process services such as wallet backend, node, etc.)
simulatorHandlers :: EffectHandlers (Builtin TestContracts) (SimulatorState (Builtin TestContracts))
simulatorHandlers = mkSimulatorHandlers def def handler where
    handler :: SimulatorContractHandler (Builtin TestContracts)
    handler = interpret (contractHandler handleBuiltin)
