module MetadataTab.Types where

import Simeon.Extended (ContractType)
import Simeon.Extended.Metadata (NumberFormat)
import Simeon.Semantics as S

class ShowConstructor a where
  showConstructor :: a -> String

data MetadataAction
  = SetContractName String
  | SetContractType ContractType
  | SetContractShortDescription String
  | SetContractLongDescription String
  | SetRoleDescription S.TokenName String
  | DeleteRoleDescription S.TokenName
  | SetSlotParameterDescription String String
  | DeleteSlotParameterDescription String
  | SetValueParameterDescription String String
  | SetValueParameterFormat String NumberFormat
  | DeleteValueParameterInfo String
  | SetChoiceDescription String String
  | SetChoiceFormat String NumberFormat
  | DeleteChoiceInfo String

instance metadataActionShowConstructor :: ShowConstructor MetadataAction where
  showConstructor (SetContractName _) = "SetContractName"
  showConstructor (SetContractType _) = "SetContractType"
  showConstructor (SetContractShortDescription _) = "SetContractShortDescription"
  showConstructor (SetContractLongDescription _) = "SetContractLongDescription"
  showConstructor (SetRoleDescription _ _) = "SetRoleDescription"
  showConstructor (DeleteRoleDescription _) = "DeleteRoleDescription"
  showConstructor (SetSlotParameterDescription _ _) = "SetSlotParameterDescription"
  showConstructor (DeleteSlotParameterDescription _) = "DeleteSlotParameterDescription"
  showConstructor (SetValueParameterDescription _ _) = "SetValueParameterDescription"
  showConstructor (SetValueParameterFormat _ _) = "SetValueParameterFormat"
  showConstructor (DeleteValueParameterInfo _) = "DeleteValueParameterInfo"
  showConstructor (SetChoiceDescription _ _) = "SetChoiceDescription"
  showConstructor (SetChoiceFormat _ _) = "SetChoiceFormat"
  showConstructor (DeleteChoiceInfo _) = "DeleteChoiceInfo"
