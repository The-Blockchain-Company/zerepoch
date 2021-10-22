{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
This module re-exports the module 'Zerepoch.V1.Ledger.Scripts', but with
additional functionality.

This module contains orphan instances of 'Bcc.Api.HasTextEnvelope', since
the Bcc Node CLI expects serialised binary values to be wrapped with a
'Bcc.Api.TextEnvelope'.
-}
module Ledger.Scripts (
    module Export
    , datumHash
    , redeemerHash
    , validatorHash
    , mintingPolicyHash
    , stakeValidatorHash
    , toBccApiScript
    , scriptHash
    ) where

import           Bcc.Api              (AsType, HasTextEnvelope (textEnvelopeType), HasTypeProxy (proxyToAsType),
                                           SerialiseAsCBOR, TextEnvelopeType (TextEnvelopeType))
import qualified Bcc.Api              as Script
import qualified Bcc.Api.Sophie      as Script
import           Bcc.Binary           (FromCBOR (fromCBOR), ToCBOR (toCBOR))
import           Codec.Serialise          (decode, encode, serialise)
import qualified Data.ByteArray           as BA
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.ByteString.Short    as SBS
import qualified Data.Text                as Text
import           Zerepoch.V1.Ledger.Api     (zerepochDatumEnvelopeType, zerepochRedeemerEnvelopeType,
                                           zerepochScriptEnvelopeType)
import           Zerepoch.V1.Ledger.Scripts as Export
import           ZerepochTx.Builtins        as Builtins

instance HasTextEnvelope Script where
    textEnvelopeType _ = TextEnvelopeType $ Text.unpack zerepochScriptEnvelopeType

instance SerialiseAsCBOR Script

instance FromCBOR Script where
    fromCBOR = decode

instance ToCBOR Script where
    toCBOR = encode

instance HasTypeProxy Script where
    data AsType Script = AsScript
    proxyToAsType _ = AsScript

instance HasTextEnvelope Datum where
  textEnvelopeType _ = TextEnvelopeType $ Text.unpack zerepochDatumEnvelopeType

instance SerialiseAsCBOR Datum

instance FromCBOR Datum where
    fromCBOR = decode

instance ToCBOR Datum where
    toCBOR = encode

instance HasTypeProxy Datum where
    data AsType Datum = AsDatum
    proxyToAsType _ = AsDatum

instance HasTextEnvelope Redeemer where
  textEnvelopeType _ = TextEnvelopeType $ Text.unpack zerepochRedeemerEnvelopeType

instance SerialiseAsCBOR Redeemer

instance FromCBOR Redeemer where
    fromCBOR = decode

instance ToCBOR Redeemer where
    toCBOR = encode

instance HasTypeProxy Redeemer where
    data AsType Redeemer = AsRedeemer
    proxyToAsType _ = AsRedeemer

datumHash :: Datum -> DatumHash
datumHash = DatumHash . Builtins.sha2_256 . BA.convert

redeemerHash :: Redeemer -> RedeemerHash
redeemerHash = RedeemerHash . Builtins.sha2_256 . BA.convert

validatorHash :: Validator -> ValidatorHash
validatorHash = ValidatorHash . scriptHash . getValidator

mintingPolicyHash :: MintingPolicy -> MintingPolicyHash
mintingPolicyHash = MintingPolicyHash . scriptHash . getMintingPolicy

stakeValidatorHash :: StakeValidator -> StakeValidatorHash
stakeValidatorHash = StakeValidatorHash . scriptHash . getStakeValidator

-- | Hash a 'Script'
scriptHash :: Script -> Builtins.BuiltinByteString
scriptHash =
    toBuiltin
    . Script.serialiseToRawBytes
    . Script.hashScript
    . toBccApiScript

-- | Convert a 'Script' to a 'bcc-api' script
toBccApiScript :: Script -> Script.Script Script.ZerepochScriptV1
toBccApiScript =
    Script.ZerepochScript Script.ZerepochScriptV1
    . Script.ZerepochScriptSerialised
    . SBS.toShort
    . BSL.toStrict
    . serialise
