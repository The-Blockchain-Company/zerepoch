{-# LANGUAGE FlexibleContexts  #-}

module ZerepochTx.Compiler.Type where

import ZerepochTx.Compiler.Types
import ZerepochTx.PIRTypes

import qualified GhcPlugins                               as GHC

compileTypeNorm :: Compiling uni fun m => GHC.Type -> m (PIRType uni)
compileType :: Compiling uni fun m => GHC.Type -> m (PIRType uni)

getMatchInstantiated :: Compiling uni fun m => GHC.Type -> m (PIRTerm uni fun)
