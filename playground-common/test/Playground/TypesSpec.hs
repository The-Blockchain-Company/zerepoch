{-# LANGUAGE OverloadedStrings #-}

module Playground.TypesSpec
    ( tests
    ) where

import           Data.Aeson       (encode, object, toJSON)
import           Playground.Types (bccCurrency)
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests = knownCurrenciesSpec

knownCurrenciesSpec :: TestTree
knownCurrenciesSpec =
    testGroup
        "mkKnownCurrencies"
        [ testCase "Serialisation" $ do
              assertEqual
                  "Should serialise Bcc properly"
                  (encode bccCurrency)
                  -- note that the object is the same as
                  -- zerepoch-playground-client\test\known_currency.json
                  (encode
                       (object
                            [ ("hash", "")
                            , ("friendlyName", "Bcc")
                            , ( "knownTokens"
                              , toJSON [object [("unTokenName", "")]])
                            ]))
        ]
