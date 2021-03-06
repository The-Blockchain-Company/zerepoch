module ZerepochIR.Analysis.Size
    ( Size (..)
    , kindSize
    , typeSize
    , tyVarDeclSize
    , termSize
    , varDeclSize
    ) where

import           ZerepochPrelude

import           ZerepochIR.Core

import           ZerepochCore.Size (Size (..), kindSize, tyVarDeclSize, typeSize, varDeclSize)

import           Control.Lens

-- | Count the number of AST nodes in a term.
termSize :: Term tyname name uni fun ann -> Size
termSize term = fold
    [ Size 1
    , term ^. termSubkinds . to kindSize
    , term ^. termSubtypes . to typeSize
    , term ^. termSubterms . to termSize
    ]
