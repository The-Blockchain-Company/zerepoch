{-# LANGUAGE NamedFieldPuns #-}

module SimulationUtils where

import           Ledger.Scripts        (ValidatorHash (ValidatorHash))
import           Ledger.Value          (CurrencySymbol (CurrencySymbol), TokenName, Value)
import qualified Ledger.Value          as Value
import           Playground.Types      (ContractCall (CallEndpoint), FunctionSchema (FunctionSchema),
                                        KnownCurrency (KnownCurrency), SimulatorAction,
                                        SimulatorWallet (SimulatorWallet), argument, argumentValues, caller,
                                        endpointDescription, hash, knownTokens, simulatorWalletBalance,
                                        simulatorWalletWallet)
import           Schema                (ToArgument, toArgument)
import           Wallet.Emulator.Types (WalletNumber)
import           Wallet.Types          (EndpointDescription)

callEndpoint :: ToArgument a => WalletNumber -> EndpointDescription -> a -> SimulatorAction
callEndpoint caller endpointDescription param =
    CallEndpoint
        { caller
        , argumentValues =
              FunctionSchema {endpointDescription, argument = toArgument param}
        }

initialBalance :: [KnownCurrency] -> Integer -> Value
initialBalance currencies balance = foldMap withCurrencies currencies
  where
    withCurrencies :: KnownCurrency -> Value
    withCurrencies KnownCurrency {hash = ValidatorHash hash, knownTokens} =
        foldMap withTokens knownTokens
      where
        currencySymbol = CurrencySymbol hash
        withTokens :: TokenName -> Value
        withTokens tokenName = Value.singleton currencySymbol tokenName balance

simulatorWallet :: [KnownCurrency] -> Integer -> WalletNumber -> SimulatorWallet
simulatorWallet currencies balance wallet =
    SimulatorWallet
        { simulatorWalletWallet = wallet
        , simulatorWalletBalance = initialBalance currencies balance
        }
