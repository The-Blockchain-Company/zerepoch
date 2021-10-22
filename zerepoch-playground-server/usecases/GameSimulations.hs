{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module GameSimulations where

import           Game                  (GuessParams (GuessParams), LockParams (LockParams), amount, guessWord,
                                        registeredKnownCurrencies, secretWord)
import qualified Ledger.Bcc            as Bcc
import           Playground.Types      (ContractCall (AddBlocks), Simulation (Simulation), SimulatorAction,
                                        simulationActions, simulationId, simulationName, simulationWallets)
import           SimulationUtils       (callEndpoint, simulatorWallet)
import           Wallet.Emulator.Types (WalletNumber (..))

simulations :: [Simulation]
simulations = [basicGame, badGuess]
  where
    wallet1 = WalletNumber 1
    wallet2 = WalletNumber 2
    wallet3 = WalletNumber 3
    basicGame =
        Simulation
            { simulationName = "Basic Game"
            , simulationId = 1
            , simulationWallets = simulatorWallet registeredKnownCurrencies 100_000_000 <$> [wallet1, wallet2]
            , simulationActions =
                  [ lock wallet1 "Zerepoch" 50_000_000
                  , AddBlocks 1
                  , guess wallet2 "Zerepoch"
                  , AddBlocks 1
                  ]
            }
    badGuess =
        Simulation
            { simulationName = "One Bad Guess"
            , simulationId = 2
            , simulationWallets = simulatorWallet registeredKnownCurrencies 100_000_000 <$> [wallet1, wallet2, wallet3]
            , simulationActions =
                  [ lock wallet1 "Zerepoch" 50_000_000
                  , AddBlocks 1
                  , guess wallet2 "Simeon"
                  , AddBlocks 1
                  , guess wallet3 "Zerepoch"
                  , AddBlocks 1
                  ]
            }

lock :: WalletNumber -> String -> Integer -> SimulatorAction
lock caller secretWord balance =
    callEndpoint
        caller
        "lock"
        LockParams {secretWord, amount = Bcc.entropicValueOf balance}

guess :: WalletNumber -> String -> SimulatorAction
guess caller guessWord = callEndpoint caller "guess" (GuessParams {guessWord})
