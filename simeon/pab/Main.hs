{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
module Main(main) where

import           SimeonContract                     (SimeonContract)
import qualified Zerepoch.PAB.Effects.Contract.Builtin as Builtin
import           Zerepoch.PAB.Run                      (runWith)

main :: IO ()
main = runWith (Builtin.handleBuiltin @SimeonContract)
