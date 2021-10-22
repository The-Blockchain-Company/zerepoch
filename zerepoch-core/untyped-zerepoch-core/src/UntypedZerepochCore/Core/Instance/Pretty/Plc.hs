{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module UntypedZerepochCore.Core.Instance.Pretty.Plc () where

import           ZerepochPrelude

import           UntypedZerepochCore.Core.Instance.Pretty.Classic  ()
import           UntypedZerepochCore.Core.Instance.Pretty.Readable ()
import           UntypedZerepochCore.Core.Type

import           ZerepochCore.Pretty.Plc

deriving via PrettyAny (Term name uni fun ann)
    instance DefaultPrettyPlcStrategy (Term name uni fun ann) =>
        PrettyBy PrettyConfigPlc (Term name uni fun ann)
deriving via PrettyAny (Program name uni fun ann)
    instance DefaultPrettyPlcStrategy (Program name uni fun ann) =>
        PrettyBy PrettyConfigPlc (Program name uni fun ann)
