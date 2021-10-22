{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import qualified Spec.Simeon.AutoExecute
import qualified Spec.Simeon.Simeon

import           Test.Tasty
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Simeon"
    [ testGroup "Contracts" [ Spec.Simeon.Simeon.tests
                            , Spec.Simeon.AutoExecute.tests
-- Does not work when invoking it from nix
--                            , testProperty "Correct Show instance for Contract"
--                                           Spec.Simeon.Simeon.prop_showWorksForContracts
                            ]
    , testGroup "Static Analysis"
        [ testProperty "No false positives" Spec.Simeon.Simeon.prop_noFalsePositives
        ]
    , testGroup "Simeon JSON"
        [ testProperty "Serialise deserialise loops" Spec.Simeon.Simeon.prop_jsonLoops
        ]
    ]
