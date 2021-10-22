module Simeon.Gists
  ( mkNewGist
  , isPlaygroundGist
  , playgroundFiles
  , filenames
  , fileExists
  , PlaygroundFiles
  ) where

import Prelude
import Blockly.Internal (XML)
import Data.Array (catMaybes)
import Data.Lens (has, view)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap, wrap)
import Gist (Gist, NewGist(NewGist), NewGistFile(..), gistFileContent, gistFiles)

mkNewGist :: String -> PlaygroundFiles -> NewGist
mkNewGist description files =
  NewGist
    { _newGistDescription: description
    , _newGistPublic: true
    , _newGistFiles: toArray files
    }

mkNewGistFile :: String -> String -> NewGistFile
mkNewGistFile _newGistFilename _newGistFileContent =
  NewGistFile
    { _newGistFilename
    , _newGistFileContent
    }

type PlaygroundFiles
  = { playground :: String
    , simeon :: Maybe String
    , haskell :: Maybe String
    , blockly :: Maybe String
    , javascript :: Maybe String
    , actus :: Maybe XML
    , metadata :: Maybe String
    }

toArray :: PlaygroundFiles -> Array NewGistFile
toArray { playground, simeon, haskell, blockly, javascript, actus, metadata } =
  [ mkNewGistFile filenames.playground playground
  ]
    <> catMaybes
        [ mkNewGistFile filenames.simeon <$> simeon
        , mkNewGistFile filenames.haskell <$> haskell
        , mkNewGistFile filenames.blockly <$> blockly
        , mkNewGistFile filenames.javascript <$> javascript
        , mkNewGistFile filenames.actus <<< unwrap <$> actus
        , mkNewGistFile filenames.metadata <$> metadata
        ]

filenames ::
  { playground :: String
  , simeon :: String
  , haskell :: String
  , blockly :: String
  , javascript :: String
  , actus :: String
  , metadata :: String
  }
filenames =
  { playground: "playground.simeon.json"
  , simeon: "playground.simeon"
  , haskell: "Main.hs"
  , blockly: "playground.simeon"
  , javascript: "playground.js"
  , actus: "actus.xml"
  , metadata: "metadata.json"
  }

isPlaygroundGist :: Gist -> Boolean
isPlaygroundGist = has (gistFiles <<< ix filenames.playground)

playgroundFiles :: Gist -> PlaygroundFiles
playgroundFiles gist =
  { playground: fromMaybe "{}" $ getFile filenames.playground
  , simeon: getFile filenames.simeon
  , haskell: getFile filenames.haskell
  , blockly: getFile filenames.blockly
  , javascript: getFile filenames.javascript
  , actus: wrap <$> getFile filenames.actus
  , metadata: getFile filenames.metadata
  }
  where
  getFile name = view (gistFiles <<< ix name <<< gistFileContent) gist

fileExists :: String -> Gist -> Boolean
fileExists name gist = has (gistFiles <<< ix name) gist
