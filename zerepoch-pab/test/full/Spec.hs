module Main
    ( main
    ) where

import qualified Zerepoch.PAB.CoreSpec
import qualified Zerepoch.PAB.Effects.Contract.BuiltinSpec
import           Test.Tasty                              (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
    testGroup
        "all tests"
        [ Zerepoch.PAB.CoreSpec.tests
        , Zerepoch.PAB.Effects.Contract.BuiltinSpec.tests
        ]
