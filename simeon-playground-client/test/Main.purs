module Test.Main where

import Prelude
import BridgeTests as BridgeTests
import Effect (Effect)
import Simeon.BlocklyTests as BlocklyTests
import Simeon.ContractTests as ContractTests
import Simeon.LintTests as LintTests
import Simeon.ParserTests as ParserTests
import Simeon.DeinstantiatorTests as DeinstantiatorTests
import Simeon.Holes.SemanticTest as HolesSemanticTest
import Simeon.Holes.TemplateTest as HolesTemplateTest
import Simeon.Holes.TimeoutTest as HolesTimeoutTest
import Test.Unit.Main (runTest)

foreign import forDeps :: Effect Unit

main :: Effect Unit
main =
  runTest do
    BridgeTests.all
    ParserTests.all
    ContractTests.all
    BlocklyTests.all
    LintTests.all
    DeinstantiatorTests.all
    HolesSemanticTest.all
    HolesTemplateTest.all
    HolesTimeoutTest.all
