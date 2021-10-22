{-# LANGUAGE TypeApplications #-}

module Main
    ( main
    ) where

import           ContractExample                     (ExampleContracts)
import qualified Zerepoch.PAB.Effects.Contract.Builtin as Builtin
import           Zerepoch.PAB.Run                      (runWith)

main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @ExampleContracts)
