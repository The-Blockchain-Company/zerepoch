{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Types
  ( Config(..)
  ) where

import qualified Auth
import           Data.Aeson     (FromJSON, parseJSON, withObject, (.:))
import qualified Simeon.Config as MC

data Config = Config
  { _authConfig    :: Auth.Config
  , _simeonConfig :: MC.Config
  }

instance FromJSON Config where
  parseJSON =
    withObject "config" $ \o -> do
      _authConfig <- o .: "auth"
      _simeonConfig <- o .: "simeon"
      pure Config {..}
