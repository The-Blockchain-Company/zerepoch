{-# LANGUAGE OverloadedStrings #-}
module Swap where

import           Language.Simeon.Extended

main :: IO ()
main = print . pretty $ contract

-- We can set explicitRefunds True to run Close refund analysis
-- but we get a shorter contract if we set it to False
explicitRefunds :: Bool
explicitRefunds = False

entropicPerBcc, amountOfBcc, amountOfEntropic, amountOfDollars :: Value
entropicPerBcc = Constant 1000000
amountOfBcc = ConstantParam "Amount of Bcc"
amountOfEntropic = MulValue entropicPerBcc amountOfBcc
amountOfDollars = ConstantParam "Amount of dollars"

bccDepositTimeout, dollarDepositTimeout :: Timeout
bccDepositTimeout = SlotParam "Timeout for Bcc deposit"
dollarDepositTimeout = SlotParam "Timeout for dollar deposit"

dollars :: Token
dollars = Token "85bb65" "dollar"

data SwapParty = SwapParty { party    :: Party
                           , currency :: Token
                           , amount   :: Value
                           }

bccProvider, dollarProvider :: SwapParty
bccProvider = SwapParty { party = Role "Bcc provider"
                        , currency = bcc
                        , amount = amountOfEntropic
                        }
dollarProvider = SwapParty { party = Role "Dollar provider"
                           , currency = dollars
                           , amount = amountOfDollars
                           }

makeDeposit :: SwapParty -> Timeout -> Contract -> Contract -> Contract
makeDeposit src timeout timeoutContinuation continuation =
  When [ Case (Deposit (party src) (party src) (currency src) (amount src))
              continuation
       ] timeout
         timeoutContinuation

refundSwapParty :: SwapParty -> Contract
refundSwapParty swapParty
  | explicitRefunds = Pay (party swapParty) (Party (party swapParty)) (currency swapParty) (amount swapParty) Close
  | otherwise = Close

makePayment :: SwapParty -> SwapParty -> Contract -> Contract
makePayment src dest =
  Pay (party src) (Party $ party dest) (currency src) (amount src)

contract :: Contract
contract = makeDeposit bccProvider bccDepositTimeout Close
         $ makeDeposit dollarProvider dollarDepositTimeout (refundSwapParty bccProvider)
         $ makePayment bccProvider dollarProvider
         $ makePayment dollarProvider bccProvider
           Close
