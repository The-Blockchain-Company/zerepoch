{- | Benchmarking for the Zerepoch versions of the Zerepoch nofib benchmarks. -}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Main (main) where

import           Common

import           Criterion.Main

import qualified Zerepoch.Benchmark.Clausify as Clausify
import qualified Zerepoch.Benchmark.Knights  as Knights
import qualified Zerepoch.Benchmark.Prime    as Prime
import qualified Zerepoch.Benchmark.Queens   as Queens

benchClausify :: Clausify.StaticFormula -> Benchmarkable
benchClausify f = nf Clausify.runClausify f

benchKnights :: Integer -> Integer -> Benchmarkable
benchKnights depth sz = nf (Knights.runKnights depth) sz

benchPrime :: Prime.PrimeID -> Benchmarkable
benchPrime pid = nf Prime.runFixedPrimalityTest pid

benchQueens :: Integer -> Queens.Algorithm -> Benchmarkable
benchQueens sz alg = nf (Queens.runQueens sz) alg

main :: IO ()
main = do
  let runners = (benchClausify, benchKnights, benchPrime, benchQueens)
  config <- Common.getConfig 5.0  -- Run each benchmark for at least five seconds
  defaultMainWith config $ Common.mkBenchMarks runners
