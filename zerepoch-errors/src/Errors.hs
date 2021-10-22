{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
-- | The catalogue of all Zerepoch errors, obsolete or not.
module Errors (allErrors) where

import           ErrorCode
import           Language.Haskell.TH                      as TH

import qualified ZerepochCore.DeBruijn                      as PLC
import qualified ZerepochCore.Error                         as PLC
import qualified ZerepochCore.Evaluation.Machine.Exception  as PLC
import qualified ZerepochIR.Error                           as PIR
import qualified ZerepochTx.Code                            as PTX
import qualified ZerepochTx.Compiler.Error                  as PTX
import qualified ZerepochTx.Lift.Class                      as PTX
import qualified UntypedZerepochCore.Evaluation.Machine.Cek as PLC

{- | A collection of error instances which are obsolete, together with their error codes bundled to one instance.
See zerepoch-errors/README.md
-}
{-# WARNING ObsoleteErrors "These errors and their error codes *should* not be thrown by any zerepoch code anymore" #-}
data ObsoleteErrors =
    ReservedErrorCode
    -- append here the obsolete errors

instance HasErrorCode ObsoleteErrors where
    errorCode ReservedErrorCode {} = ErrorCode 0
    -- append here the corresponding obsolete error codes

-- | All errors among the whole Zerepoch project. This includes both existing and obsolete errors.
-- Note: the order of adding to this list does not matter, except for haddock looks.
allErrors :: [TH.Name]
allErrors =
   [ 'ReservedErrorCode
   , 'PIR.MalformedDataConstrResType
   , 'PIR.CompilationError
   , 'PIR.UnsupportedError
   , 'PLC.LexErr
   , 'PLC.Unexpected
   , 'PLC.UnknownBuiltinType
   , 'PLC.BuiltinTypeNotAStar
   , 'PLC.UnknownBuiltinFunction
   , 'PLC.InvalidBuiltinConstant
   , 'PLC.MultiplyDefined
   , 'PLC.IncoherentUsage
   , 'PLC.BadType
   , 'PLC.BadTerm
   , 'PLC.KindMismatch
   , 'PLC.TypeMismatch
   , 'PLC.UnknownBuiltinFunctionE
   , 'PLC.FreeTypeVariableE
   , 'PLC.FreeVariableE
   , 'PLC.FreeVariable
   , 'PLC.FreeUnique
   , 'PLC.FreeIndex
   , 'PLC.NonPolymorphicInstantiationMachineError
   , 'PLC.NonWrapUnwrappedMachineError
   , 'PLC.NonFunctionalApplicationMachineError
   , 'PLC.OpenTermEvaluatedMachineError
   , 'PLC.UnliftingErrorE
   , 'PLC.BuiltinTermArgumentExpectedMachineError
   , 'PLC.UnexpectedBuiltinTermArgumentMachineError
   , 'PLC.EmptyBuiltinArityMachineError
   , 'PLC.CekOutOfExError
   , 'PLC.CekEvaluationFailure
   , 'PTX.ImpossibleDeserialisationFailure
   , 'PTX.CompilationError
   , 'PTX.UnsupportedError
   , 'PTX.FreeVariableError
   , 'PTX.InvalidMarkerError
   , 'PTX.CoreNameLookupError
   , 'PTX.UnsupportedLiftType
   , 'PTX.UnsupportedLiftKind
   , 'PTX.UserLiftError
   , 'PTX.LiftMissingDataCons
   , 'PTX.LiftMissingVar
   ]
