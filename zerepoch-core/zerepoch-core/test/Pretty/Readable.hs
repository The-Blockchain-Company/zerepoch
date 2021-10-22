module Pretty.Readable (test_Pretty) where

import           ZerepochCore.Default
import           ZerepochCore.FsTree
import           ZerepochCore.Pretty

import           ZerepochCore.Examples.Everything (examples)
import           ZerepochCore.StdLib.Everything   (stdLib)

import           Common

import           Test.Tasty

prettyConfigReadable :: PrettyConfigPlc
prettyConfigReadable
    = PrettyConfigPlc defPrettyConfigPlcOptions
    . PrettyConfigPlcReadable
    $ botPrettyConfigReadable defPrettyConfigName ShowKindsYes

testReadable :: PrettyPlc a => TestName -> a -> TestNested
testReadable name = nestedGoldenVsDoc name . prettyBy prettyConfigReadable

test_PrettyReadable :: TestTree
test_PrettyReadable =
    testGroup "Bundles"
        [ folder stdLib
        , folder examples
        ]
  where
    folder :: Pretty fun => PlcFolderContents DefaultUni fun -> TestTree
    folder
        = runTestNestedIn ["zerepoch-core", "test", "Pretty", "Golden"]
        . testNested "Readable"
        . foldPlcFolderContents testNested testReadable testReadable

test_Pretty :: TestTree
test_Pretty =
    testGroup "pretty"
        [ test_PrettyReadable
        ]
