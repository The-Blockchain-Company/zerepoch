{-# LANGUAGE TypeApplications #-}

module NamesSpec
    ( names
    ) where

import           PlcTestUtils

import           ZerepochIR.Generators.AST
import           ZerepochIR.Mark
import           ZerepochIR.Transform.Rename

import           ZerepochCore.Rename

import           Test.Tasty

names :: TestTree
names =
    testGroup "names"
        [ test_scopingGood genProgram rename
        , test_scopingBad genProgram markNonFreshProgram renameProgramM
        ]
