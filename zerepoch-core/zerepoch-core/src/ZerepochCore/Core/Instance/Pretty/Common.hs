{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

module ZerepochCore.Core.Instance.Pretty.Common () where

import           ZerepochPrelude

import           ZerepochCore.Core.Type

instance Pretty (Version ann) where
    pretty (Version _ i j k) = pretty i <> "." <> pretty j <> "." <> pretty k
