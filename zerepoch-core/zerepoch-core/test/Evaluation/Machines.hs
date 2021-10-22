{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

module Evaluation.Machines
    ( test_machines
    )
where

import           ZerepochCore
import           ZerepochCore.Evaluation.Machine.Ck
import           ZerepochCore.Evaluation.Machine.Exception
import           ZerepochCore.Generators.Interesting
import           ZerepochCore.Generators.Test
import           ZerepochCore.Pretty


import           Test.Tasty
import           Test.Tasty.Hedgehog

testMachine
    :: (uni ~ DefaultUni, fun ~ DefaultFun, PrettyPlc internal)
    => String
    -> (Term TyName Name uni fun () ->
           Either (EvaluationException user internal (Term TyName Name uni fun ())) (Term TyName Name uni fun ()))
    -> TestTree
testMachine machine eval =
    testGroup machine $ fromInterestingTermGens $ \name ->
        testProperty name . propEvaluate eval

test_machines :: TestTree
test_machines = testGroup
    "machines"
    [ testMachine "CK" $ evaluateCkNoEmit defaultBuiltinsRuntime
    ]
