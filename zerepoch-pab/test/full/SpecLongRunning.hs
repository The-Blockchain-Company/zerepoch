module Main
    ( main
    ) where

import qualified Zerepoch.PAB.CliSpec
import           Test.Tasty         (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
    testGroup
        "all tests"
        [ Zerepoch.PAB.CliSpec.tests
        ]
