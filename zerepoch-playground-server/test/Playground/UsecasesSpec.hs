{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module Playground.UsecasesSpec
    ( tests
    ) where

import           Control.Monad                (unless)
import           Control.Monad.Except         (runExceptT)
import           Control.Monad.Except.Extras  (mapError)
import           Control.Newtype.Generics     (over)
import           Crowdfunding                 (Contribution (Contribution), contribValue)
import           Data.Aeson                   (ToJSON)
import qualified Data.Aeson                   as JSON
import qualified Data.Aeson.Text              as JSON
import           Data.Foldable                (traverse_)
import           Data.List                    (isPrefixOf)
import           Data.List.NonEmpty           (NonEmpty ((:|)))
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import qualified Data.Text.Lazy               as TL
import           Data.Time.Units              (Minute)
import           Game                         (GuessParams (GuessParams), LockParams (LockParams), amount, guessWord,
                                               secretWord)
import qualified Interpreter                  as Webghc
import           Language.Haskell.Interpreter (InterpreterError, InterpreterResult (InterpreterResult, result),
                                               SourceCode (SourceCode))
import           Ledger.Bcc                   (bccValueOf, entropicValueOf)
import           Ledger.Blockchain            (OnChainTx (..))
import           Ledger.Scripts               (ValidatorHash (ValidatorHash))
import           Ledger.Value                 (TokenName (TokenName), Value)
import qualified Playground.Interpreter       as PI
import           Playground.Types             (CompilationResult (CompilationResult),
                                               ContractCall (AddBlocks, AddBlocksUntil, CallEndpoint, PayToWallet),
                                               Evaluation (Evaluation), EvaluationResult (EvaluationResult), Expression,
                                               FunctionSchema (FunctionSchema), KnownCurrency (KnownCurrency),
                                               PlaygroundError (InterpreterError), SimulatorWallet (SimulatorWallet),
                                               bccCurrency, argument, argumentValues, caller, emulatorLog,
                                               endpointDescription, feesDistribution, fundsDistribution, program,
                                               resultRollup, simulatorWalletBalance, simulatorWalletWallet, sourceCode,
                                               walletKeys, wallets)
import           Playground.Usecases          (crowdFunding, errorHandling, game, vesting)
import           Schema                       (FormSchema (FormSchemaUnit, FormSchemaValue))
import           System.Environment           (lookupEnv)
import           Test.Tasty                   (TestTree, testGroup)
import           Test.Tasty.HUnit             (Assertion, assertBool, assertEqual, assertFailure, testCase)
import           Wallet.Emulator.Types        (WalletNumber (..), fromWalletNumber)
import           Wallet.Rollup.Render         (showBlockchain)
import           Wallet.Rollup.Types          (AnnotatedTx (..))
import           Wallet.Types                 (EndpointDescription (EndpointDescription))

tests :: TestTree
tests =
    testGroup
        "Playground.Usecases"
        [ runningInNixBuildTest
        , vestingTest
        , gameTest
        , errorHandlingTest
        , crowdfundingTest
        , knownCurrencyTest
        ]

maxInterpretationTime :: Minute
maxInterpretationTime = 2

w1, w2, w3, w4, w5 :: WalletNumber
w1 = WalletNumber 1

w2 = WalletNumber 2

w3 = WalletNumber 3

w4 = WalletNumber 4

w5 = WalletNumber 5

mkSimulatorWallet :: WalletNumber -> Value -> SimulatorWallet
mkSimulatorWallet simulatorWalletWallet simulatorWalletBalance =
    SimulatorWallet {..}

--  Unfortunately it's currently not possible to get these tests to work outside of a nix build.
--  Running `cabal test` will yield a lot of import errors because of missing modules.
runningInNixBuildTest :: TestTree
runningInNixBuildTest =
    testGroup
        "nixBuild"
        [ testCase "needs to be executed via nix-build" $ do
            nixBuildTop <- fromMaybe "" <$> lookupEnv "NIX_BUILD_TOP"
            assertBool "UsecasesSpec will only work when executed as part of a nix build" (nixBuildTop == "/build" || "/private/tmp/nix-build" `isPrefixOf` nixBuildTop)
        ]

vestingTest :: TestTree
vestingTest =
    testGroup
        "vesting"
        [ compilationChecks vesting
        , testCase "should compile with the expected schema" $ do
              Right (InterpreterResult _ (CompilationResult result _)) <-
                  compile vesting
              assertEqual
                  ""
                  [ FunctionSchema
                        { endpointDescription = EndpointDescription "retrieve funds"
                        , argument = FormSchemaValue
                        }
                  , FunctionSchema
                        { endpointDescription = EndpointDescription "vest funds"
                        , argument = FormSchemaUnit
                        }
                  ]
                  result
        , testCase "should run simple evaluation" $
          evaluate (mkEvaluation []) >>=
          hasFundsDistribution
              [ mkSimulatorWallet w1 hundredMEntropic
              , mkSimulatorWallet w2 hundredMEntropic
              ]
        , testCase "should run simple wait evaluation" $
          evaluate (mkEvaluation [AddBlocks 10]) >>=
          hasFundsDistribution
              [ mkSimulatorWallet w1 hundredMEntropic
              , mkSimulatorWallet w2 hundredMEntropic
              ]
        , testCase "should run vest funds evaluation" $
          evaluate vestFundsEval >>=
          hasFundsDistribution
              [ mkSimulatorWallet w1 hundredMEntropic
              , mkSimulatorWallet w2 $ entropicValueOf 20_000_000
              ]
        , testCase "should run vest and a partial retrieve of funds" $
          evaluate vestAndPartialRetrieveEval >>=
          hasFundsDistribution
              [ mkSimulatorWallet w1 $ entropicValueOf 150_000_000
              , mkSimulatorWallet w2 $ entropicValueOf 20_000_000
              ]
        , testCase "should run vest and a full retrieve of funds" $
          evaluate vestAndFullRetrieveEval >>=
          hasFundsDistribution
              [ mkSimulatorWallet w1 $ entropicValueOf 180_000_000
              , mkSimulatorWallet w2 $ entropicValueOf 20_000_000
              ]
        ]
  where
    hundredMEntropic = entropicValueOf 100_000_000
    mkEvaluation :: [Expression] -> Evaluation
    mkEvaluation expressions =
        Evaluation
            { wallets =
                  [ mkSimulatorWallet w1 hundredMEntropic
                  , mkSimulatorWallet w2 hundredMEntropic
                  ]
            , sourceCode = vesting
            , program = toJSONString expressions
            }
    vestFundsEval = mkEvaluation [vestFunds w2, AddBlocks 1]
    vestAndPartialRetrieveEval =
        mkEvaluation
            [vestFunds w2, AddBlocks 20, retrieveFunds w1 50_000_000, AddBlocks 2]
    vestAndFullRetrieveEval =
        mkEvaluation
            [vestFunds w2, AddBlocks 40, retrieveFunds w1 80_000_000, AddBlocks 5]
    vestFunds caller = callEndpoint "vest funds" caller ()
    retrieveFunds caller balance =
        callEndpoint "retrieve funds" caller $ entropicValueOf balance

gameTest :: TestTree
gameTest =
    testGroup
        "game"
        [ compilationChecks game
        , testCase "should keep the funds" $
          evaluate (mkEvaluation [lock w2 "abcde" twoBcc, AddBlocks 1, guess w1 "ade", AddBlocks 1]) >>=
          hasFundsDistribution
              [ mkSimulatorWallet w1 tenBcc
              , mkSimulatorWallet w2 (bccValueOf 8)
              ]
        , testCase "should unlock the funds" $
          evaluate (mkEvaluation [lock w2 "abcde" twoBcc, AddBlocks 1, guess w1 "abcde", AddBlocks 1]) >>=
          hasFundsDistribution
              [ mkSimulatorWallet w1 (bccValueOf 12)
              , mkSimulatorWallet w2 (bccValueOf 8)
              ]
        , testCase "Sequential fund transfer - PayToWallet" $
          evaluate (payAll w3 w4 w5) >>=
          hasFundsDistribution
              [ mkSimulatorWallet w3 tenBcc
              , mkSimulatorWallet w4 tenBcc
              , mkSimulatorWallet w5 tenBcc
              ]
        , testCase "Sequential fund transfer - PayToWallet" $
          evaluate (payAll w1 w2 w3) >>=
          hasFundsDistribution
              [ mkSimulatorWallet w1 tenBcc
              , mkSimulatorWallet w2 tenBcc
              , mkSimulatorWallet w3 tenBcc
              ]
        ]
  where
    tenBcc = bccValueOf 10
    sourceCode = game
    wallets = [mkSimulatorWallet w1 tenBcc, mkSimulatorWallet w2 tenBcc]
    mkEvaluation :: [Expression] -> Evaluation
    mkEvaluation expressions =
        Evaluation
            {sourceCode = game, wallets, program = toJSONString expressions}
    lock caller secretWord amount =
        callEndpoint "lock" caller $ LockParams {secretWord, amount}
    guess caller guessWord =
        callEndpoint "guess" caller $ GuessParams {guessWord}
    payAll a b c =
        Evaluation
            { sourceCode
            , wallets =
                  [ mkSimulatorWallet a tenBcc
                  , mkSimulatorWallet b tenBcc
                  , mkSimulatorWallet c tenBcc
                  ]
            , program =
                  toJSONString
                      ([ PayToWallet a b nineBcc
                       , PayToWallet b c nineBcc
                       , PayToWallet c a nineBcc
                       ] :: [Expression])
            }
    nineBcc = bccValueOf 9
    twoBcc = bccValueOf 2

hasFundsDistribution ::
       [SimulatorWallet]
    -> Either PlaygroundError (InterpreterResult EvaluationResult)
    -> Assertion
hasFundsDistribution _ (Left err) = assertFailure $ show err
hasFundsDistribution requiredDistribution (Right InterpreterResult {result = EvaluationResult {..}}) = do
    let addFees fund fee = fund { simulatorWalletBalance = simulatorWalletBalance fund <> simulatorWalletBalance fee }
    let noFeesDistribution = zipWith addFees fundsDistribution feesDistribution
    unless (requiredDistribution == noFeesDistribution) $ do
        Text.putStrLn $
            either id id $ showBlockchain
                (fmap (fmap fromWalletNumber) walletKeys)
                (fmap (fmap (\AnnotatedTx {tx, valid} -> if valid then Valid tx else Invalid tx)) resultRollup)
        traverse_ print $ reverse emulatorLog
    assertEqual "" requiredDistribution noFeesDistribution

errorHandlingTest :: TestTree
errorHandlingTest = testGroup "errorHandling" [compilationChecks errorHandling]

crowdfundingTest :: TestTree
crowdfundingTest =
    testGroup
        "crowdfunding"
        [ compilationChecks crowdFunding
        , testCase "should run successful campaign" $
          evaluate successfulCampaign >>=
          hasFundsDistribution
              [ mkSimulatorWallet w1 $ entropicValueOf 600000
              , mkSimulatorWallet w2 $ entropicValueOf 190000
              , mkSimulatorWallet w3 $ entropicValueOf 200000
              , mkSimulatorWallet w4 $ entropicValueOf 210000
              ]
        ]
  where
    sourceCode = crowdFunding
    successfulCampaign =
        Evaluation
            { wallets =
                  [ mkSimulatorWallet w1 $ entropicValueOf 300000
                  , mkSimulatorWallet w2 $ entropicValueOf 300000
                  , mkSimulatorWallet w3 $ entropicValueOf 300000
                  , mkSimulatorWallet w4 $ entropicValueOf 300000
                  ]
            , program =
                  toJSONString
                      [ scheduleCollection w1
                      , contribute w2 $ entropicValueOf 110000
                      , contribute w3 $ entropicValueOf 100000
                      , contribute w4 $ entropicValueOf 90000
                      , AddBlocks 1
                      , AddBlocksUntil 41
                      , AddBlocks 1
                      ]
            , sourceCode
            }
    scheduleCollection caller = callEndpoint "schedule collection" caller ()
    contribute caller contribValue =
        callEndpoint "contribute" caller $ Contribution {contribValue}

knownCurrencyTest :: TestTree
knownCurrencyTest =
    testCase "should return registered known currencies" $ do
        currencies <- compile source
        hasKnownCurrency currencies
  where
    source =
        SourceCode $
        Text.unlines
            [ "import Playground.Contract"
            , "import Data.Text"
            , "import Data.List.NonEmpty (NonEmpty ((:|)))"
            , "import Ledger.Value (TokenName(TokenName))"
            , "import Ledger.Scripts (ValidatorHash (..))"
            , "import Playground.Types (KnownCurrency (..))"
            , "import ZerepochTx.Prelude"
            , ""
            , "myCurrency :: KnownCurrency"
            , "myCurrency = KnownCurrency (ValidatorHash \"\") \"MyCurrency\" (TokenName \"MyToken\" :| [])"
            , "$(mkKnownCurrencies ['myCurrency])"
            , ""
            , "schemas = []"
            , "iotsDefinitions = \"\""
            ]
    expectedCurrencies =
        [ bccCurrency
        , KnownCurrency
              (ValidatorHash "")
              "MyCurrency"
              (TokenName "MyToken" :| [])
        ]
    hasKnownCurrency (Right (InterpreterResult _ (CompilationResult _ currencies))) =
        assertEqual "" expectedCurrencies currencies
    hasKnownCurrency other =
        assertFailure $ "Compilation failed: " <> show other

compile ::
       SourceCode
    -> IO (Either InterpreterError (InterpreterResult CompilationResult))
compile source = runExceptT $ do
  PI.checkCode source
  result <- Webghc.compile maxInterpretationTime False $ over SourceCode PI.mkCompileScript source
  PI.getCompilationResult result

evaluate ::
       Evaluation
    -> IO (Either PlaygroundError (InterpreterResult EvaluationResult))
evaluate evaluation = runExceptT $ do
    expr <- PI.evaluationToExpr evaluation
    result <- mapError InterpreterError $ Webghc.compile maxInterpretationTime False (SourceCode expr)
    PI.decodeEvaluation result

compilationChecks :: SourceCode -> TestTree
compilationChecks source =
    testCase "should compile" $ do
        compiled <- compile source
        case compiled of
            Left err -> assertFailure $ "Compilation failed: " <> show err
            Right _  -> pure ()

-- | Encode a value in JSON, then make a JSON *string* from that
toJSONString :: ToJSON a => a -> JSON.Value
toJSONString = JSON.String . TL.toStrict . JSON.encodeToLazyText

callEndpoint :: ToJSON a => EndpointDescription -> WalletNumber -> a -> Expression
callEndpoint endpointDescription caller param =
    CallEndpoint
        { caller
        , argumentValues =
              FunctionSchema {endpointDescription, argument = toJSONString param}
        }
