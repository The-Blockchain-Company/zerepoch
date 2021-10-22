module ZerepochCore.Pretty
    (
    -- * Basic types and functions
      Doc
    , Pretty (..)
    , PrettyBy (..)
    , IgnorePrettyConfig (..)
    , AttachPrettyConfig (..)
    , Render (..)
    , display
    , displayBy
    -- * Defaults
    , prettyPlcDef
    , displayPlcDef
    , prettyPlcDebug
    , displayPlcDebug
    -- * Global configuration
    , CondensedErrors (..)
    , PrettyConfigPlcOptions (..)
    , PrettyConfigPlcStrategy (..)
    , PrettyConfigPlc (..)
    , PrettyPlc
    , defPrettyConfigPlcOptions
    , defPrettyConfigPlcClassic
    , debugPrettyConfigPlcClassic
    , defPrettyConfigPlcReadable
    , debugPrettyConfigPlcReadable
    -- * Custom functions for PLC types.
    , prettyPlcClassicDef
    , prettyPlcClassicDebug
    , prettyPlcReadableDef
    , prettyPlcReadableDebug
    , prettyPlcCondensedErrorBy
    , displayPlcCondensedErrorClassic
    -- * Names
    , PrettyConfigName (..)
    , HasPrettyConfigName (..)
    , defPrettyConfigName
    , debugPrettyConfigName
    -- * Classic view
    , PrettyConfigClassic (..)
    , PrettyClassicBy
    , PrettyClassic
    , consAnnIf
    , prettyClassicDef
    , prettyClassicDebug
    -- * Readable view
    , ShowKinds (..)
    , PrettyConfigReadable (..)
    , PrettyReadableBy
    , PrettyReadable
    , topPrettyConfigReadable
    , botPrettyConfigReadable
    -- * Utils
    , prettyBytes
    , ConstConfig (..)
    , PrettyConst
    , prettyConst
    ) where

import           ZerepochCore.Pretty.Classic
import           ZerepochCore.Pretty.ConfigName
import           ZerepochCore.Pretty.Default
import           ZerepochCore.Pretty.Plc
import           ZerepochCore.Pretty.PrettyConst
import           ZerepochCore.Pretty.Readable
import           ZerepochCore.Pretty.Utils

import           Text.Pretty
import           Text.PrettyBy
