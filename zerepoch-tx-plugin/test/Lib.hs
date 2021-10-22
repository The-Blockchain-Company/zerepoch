{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Lib where

import           Common
import           PlcTestUtils

import           Language.Haskell.TH

import qualified ZerepochTx.Builtins          as Builtins
import           ZerepochTx.Code
import           ZerepochTx.Evaluation
import           ZerepochTx.TH

import qualified ZerepochCore                 as PLC
import           ZerepochCore.Pretty          (PrettyConst)
import qualified UntypedZerepochCore          as UPLC
import qualified UntypedZerepochCore.DeBruijn as UPLC

import           Data.Text.Prettyprint.Doc
import           Flat                       (Flat)

instance (PLC.Closed uni, uni `PLC.Everywhere` Flat, uni `PLC.Everywhere` PrettyConst, PLC.GShow uni, Pretty fun, Flat fun) =>
            ToUPlc (CompiledCodeIn uni fun a) uni fun where
    toUPlc v = do
        v' <- catchAll $ getPlc v
        toUPlc v'

goldenPir
    :: (PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PrettyConst, uni `PLC.Everywhere` Flat, PLC.GShow uni, Pretty fun, Flat fun)
    => String -> CompiledCodeIn uni fun a -> TestNested
goldenPir name value = nestedGoldenVsDoc name $ pretty $ getPir value
