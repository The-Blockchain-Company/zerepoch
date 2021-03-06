module StaticData
  ( haskellBufferLocalStorageKey
  , jsBufferLocalStorageKey
  , demoFiles
  , demoFilesJS
  , demoFilesMetadata
  , simeonBufferLocalStorageKey
  , simulatorBufferLocalStorageKey
  , simeonContract
  , simeonContracts
  , gistIdLocalStorageKey
  , sessionStorageKey
  ) where

import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Examples.Haskell.Contracts (contractForDifferences, contractForDifferencesWithOracle, couponBondGuaranteed, escrow, escrowWithCollateral, example, swap, zeroCouponBond) as HE
import Examples.JS.Contracts (contractForDifferences, contractForDifferencesWithOracle, couponBondGuaranteed, escrow, escrowWithCollateral, example, swap, zeroCouponBond) as JSE
import Examples.Simeon.Contracts (contractForDifferences, contractForDifferencesWithOracle, couponBondGuaranteed, escrow, escrowWithCollateral, example, swap, zeroCouponBond) as ME
import Examples.Metadata (contractForDifferences, contractForDifferencesWithOracle, couponBondGuaranteed, escrow, escrowWithCollateral, example, swap, zeroCouponBond) as M
import LocalStorage as LocalStorage
import Simeon.Extended.Metadata (MetaData)

type Label
  = String

type Contents
  = String

demoFiles ::
  Map Label Contents
demoFiles =
  Map.fromFoldable
    [ "Example" /\ HE.example
    , "Escrow" /\ HE.escrow
    , "EscrowWithCollateral" /\ HE.escrowWithCollateral
    , "ZeroCouponBond" /\ HE.zeroCouponBond
    , "CouponBondGuaranteed" /\ HE.couponBondGuaranteed
    , "Swap" /\ HE.swap
    , "CFD" /\ HE.contractForDifferences
    , "CFDWithOracle" /\ HE.contractForDifferencesWithOracle
    ]

demoFilesJS ::
  Map Label Contents
demoFilesJS =
  Map.fromFoldable
    [ "Example" /\ JSE.example
    , "Escrow" /\ JSE.escrow
    , "EscrowWithCollateral" /\ JSE.escrowWithCollateral
    , "ZeroCouponBond" /\ JSE.zeroCouponBond
    , "CouponBondGuaranteed" /\ JSE.couponBondGuaranteed
    , "Swap" /\ JSE.swap
    , "CFD" /\ JSE.contractForDifferences
    , "CFDWithOracle" /\ JSE.contractForDifferencesWithOracle
    ]

simeonContracts ::
  Map Label Contents
simeonContracts =
  Map.fromFoldable
    [ "Example" /\ ME.example
    , "Escrow" /\ ME.escrow
    , "EscrowWithCollateral" /\ ME.escrowWithCollateral
    , "ZeroCouponBond" /\ ME.zeroCouponBond
    , "CouponBondGuaranteed" /\ ME.couponBondGuaranteed
    , "Swap" /\ ME.swap
    , "CFD" /\ ME.contractForDifferences
    , "CFDWithOracle" /\ ME.contractForDifferencesWithOracle
    ]

demoFilesMetadata ::
  Map Label MetaData
demoFilesMetadata =
  Map.fromFoldable
    [ "Example" /\ M.example
    , "Escrow" /\ M.escrow
    , "EscrowWithCollateral" /\ M.escrowWithCollateral
    , "ZeroCouponBond" /\ M.zeroCouponBond
    , "CouponBondGuaranteed" /\ M.couponBondGuaranteed
    , "Swap" /\ M.swap
    , "CFD" /\ M.contractForDifferences
    , "CFDWithOracle" /\ M.contractForDifferencesWithOracle
    ]

simeonContract ::
  Contents
simeonContract = "(Some Simeon Code)"

haskellBufferLocalStorageKey ::
  LocalStorage.Key
haskellBufferLocalStorageKey = LocalStorage.Key "HaskellBuffer"

jsBufferLocalStorageKey ::
  LocalStorage.Key
jsBufferLocalStorageKey = LocalStorage.Key "JavascriptBuffer"

simeonBufferLocalStorageKey ::
  LocalStorage.Key
simeonBufferLocalStorageKey = LocalStorage.Key "SimeonBuffer"

simulatorBufferLocalStorageKey ::
  LocalStorage.Key
simulatorBufferLocalStorageKey = LocalStorage.Key "SimulationBuffer"

gistIdLocalStorageKey ::
  LocalStorage.Key
gistIdLocalStorageKey = LocalStorage.Key "GistId"

sessionStorageKey :: LocalStorage.Key
sessionStorageKey = LocalStorage.Key "SimeonPlaygroundSession"
