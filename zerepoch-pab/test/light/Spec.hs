module Main
    ( main
    ) where

import qualified Bcc.Api.NetworkId.ExtraSpec
import qualified Bcc.Wallet.ServerSpec
import qualified Control.Concurrent.STM.ExtrasSpec
import           Test.Tasty                        (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
    testGroup
        "all tests"
        [ Bcc.Api.NetworkId.ExtraSpec.tests
        , Bcc.Wallet.ServerSpec.tests
        , Control.Concurrent.STM.ExtrasSpec.tests
        ]
