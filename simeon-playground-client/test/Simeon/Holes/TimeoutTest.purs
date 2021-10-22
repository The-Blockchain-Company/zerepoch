module Simeon.Holes.TimeoutTest where

import Prelude
import Data.Maybe (Maybe(..))
import Simeon.Gen (genContract, GenerationOptions(..))
import Simeon.GenWithHoles (GenWithHoles, contractQuickCheck)
import Simeon.Holes (fromTerm)
import Simeon.Semantics (timeouts)
import Simeon.Semantics as S
import Test.QuickCheck (Result, (===))
import Test.Unit (TestSuite, suite, test)

all :: TestSuite
all =
  suite "Simeon.Holes.Timeout" do
    test "Term and Semantic contract has the same timeouts" $ contractQuickCheck (GenerationOptions { withHoles: false, withExtendedConstructs: false }) sameTimeouts

sameTimeouts :: GenWithHoles Result
sameTimeouts = do
  termContract <- genContract
  let
    (mSContract :: Maybe S.Contract) = fromTerm termContract

    termTimeouts = timeouts termContract

    mSemanticTimeouts = timeouts <$> mSContract
  pure (Just termTimeouts === mSemanticTimeouts)
