module ChainTests
  ( all
  ) where

import Prelude
import Data.Array (mapWithIndex)
import Data.BigInteger as BigInteger
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import ZerepochTx.AssocMap as AssocMap
import Zerepoch.V1.Ledger.Value (CurrencySymbol(..), TokenName(..), Value(..))
import Playground.Types (SimulatorWallet(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Transaction.View (extractAmount)
import Wallet.Emulator.Wallet (WalletNumber(..))

all :: TestSuite
all =
  suite "Chain" do
    extractAmountsTests

extractAmountsTests :: TestSuite
extractAmountsTests =
  suite "extractAmount" do
    test "All present"
      $ equal
          [ Just (BigInteger.fromInt 10)
          , Just (BigInteger.fromInt 40)
          , Just (BigInteger.fromInt 70)
          ]
          (map (extractAmount (currencies /\ usdToken)) wallets)
    test "All missing"
      $ equal
          [ Nothing
          , Nothing
          , Nothing
          ]
          (map (extractAmount (currencies /\ bccToken)) wallets)
    test "Mixed" do
      equal
        [ Just (BigInteger.fromInt 20)
        , Just (BigInteger.fromInt 50)
        , Nothing
        ]
        (map (extractAmount (currencies /\ eurToken)) wallets)
      equal
        [ Nothing
        , Just (BigInteger.fromInt 30)
        , Just (BigInteger.fromInt 60)
        ]
        (map (extractAmount (bcc /\ bccToken)) wallets)

wallets :: Array SimulatorWallet
wallets =
  mapWithIndex
    ( \id value ->
        SimulatorWallet
          { simulatorWalletWallet: WalletNumber { getWallet: BigInteger.fromInt id }
          , simulatorWalletBalance: value
          }
    )
    values

values :: Array Value
values =
  [ Value
      { getValue:
          AssocMap.fromTuples
            [ currencies
                /\ AssocMap.fromTuples
                    [ usdToken /\ BigInteger.fromInt 10
                    , eurToken /\ BigInteger.fromInt 20
                    ]
            ]
      }
  , Value
      { getValue:
          AssocMap.fromTuples
            [ bcc /\ AssocMap.fromTuples [ bccToken /\ BigInteger.fromInt 30 ]
            , currencies
                /\ AssocMap.fromTuples
                    [ usdToken /\ BigInteger.fromInt 40
                    , eurToken /\ BigInteger.fromInt 50
                    ]
            ]
      }
  , Value
      { getValue:
          AssocMap.fromTuples
            [ bcc /\ AssocMap.fromTuples [ bccToken /\ BigInteger.fromInt 60 ]
            , currencies
                /\ AssocMap.fromTuples
                    [ usdToken /\ BigInteger.fromInt 70
                    ]
            ]
      }
  ]

bcc :: CurrencySymbol
bcc = CurrencySymbol { unCurrencySymbol: "" }

currencies :: CurrencySymbol
currencies = CurrencySymbol { unCurrencySymbol: "Currency" }

bccToken :: TokenName
bccToken = TokenName { unTokenName: "" }

usdToken :: TokenName
usdToken = TokenName { unTokenName: "USDToken" }

eurToken :: TokenName
eurToken = TokenName { unTokenName: "EURToken" }
