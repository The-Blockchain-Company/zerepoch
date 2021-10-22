module Simeon.ParserTests where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), stripPrefix, stripSuffix, trim)
import Simeon.Gen (genContract, GenerationOptions(..))
import Simeon.GenWithHoles (GenWithHoles, contractQuickCheck)
import Simeon.Holes (Contract)
import Simeon.Parser (parseContract)
import Test.QuickCheck (Result, (===))
import Test.Unit (TestSuite, suite, test)
import Text.Pretty (genericPretty)

all :: TestSuite
all =
  suite "Simeon.Parser" do
    let
      genOpts = GenerationOptions { withHoles: false, withExtendedConstructs: true }
    test "Contract Parser" $ contractQuickCheck genOpts contractParser
    test "Pretty Contract Parser" $ contractQuickCheck genOpts prettyContractParser

contractParser :: GenWithHoles Result
contractParser = do
  v <- genContract
  let
    contractWithNoParens = fromMaybe (show v) (stripPrefix (Pattern "(") (show v) >>= stripSuffix (Pattern ")"))

    result = parseContract contractWithNoParens

    (expected :: Either String Contract) = Right v
  pure (show result === show expected)

prettyContractParser :: GenWithHoles Result
prettyContractParser = do
  v <- genContract
  let
    prettyContract = trim <<< show <<< genericPretty $ v

    contractWithNoParens = fromMaybe prettyContract (stripPrefix (Pattern "(") prettyContract >>= stripSuffix (Pattern ")"))

    result = parseContract contractWithNoParens

    (expected :: Either String Contract) = Right v
  pure (show result === show expected)
