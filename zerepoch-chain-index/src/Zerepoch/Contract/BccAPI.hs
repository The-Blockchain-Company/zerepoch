{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ViewPatterns       #-}
{-|

Interface to the transaction types from 'bcc-api'

-}
module Zerepoch.Contract.BccAPI(
    fromBccBlock
  , fromBccTx
  , fromBccTxIn
  , fromBccTxInsCollateral
  , fromBccTxInWitness
  , fromBccTxOut
  , fromBccAddress
  , fromBccMintValue
  , fromBccValue
  , fromBccFee
  , fromBccValidityRange
  , fromBccExtraScriptData
  , fromBccScriptInEra
  , toBccTxBody
  , toBccTxIn
  , toBccTxInsCollateral
  , toBccTxInWitness
  , toBccTxOut
  , toBccAddress
  , toBccMintValue
  , toBccValue
  , toBccFee
  , toBccValidityRange
  , toBccExtraScriptData
  , toBccScriptInEra
  , toBccPaymentKeyHash
  , toBccScriptHash
  , ToBccError(..)
  , FromBccError(..)
) where

import qualified Bcc.Api                    as C
import qualified Bcc.Api.Cole              as C
import qualified Bcc.Api.Sophie            as C
import           Bcc.BM.Data.Tracer         (ToObject (..))
import           Bcc.Chain.Common           (addrToBase58)
import qualified Codec.Serialise                as Codec
import           Data.Aeson                     (FromJSON, ToJSON)
import           Data.Bifunctor                 (first)
import           Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BSL
import           Data.ByteString.Short          as BSS
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import           Data.Text.Prettyprint.Doc      (Pretty (..), colon, (<+>))
import           GHC.Generics                   (Generic)
import qualified Ledger                         as P
import qualified Ledger.Bcc                     as Bcc
import           Zerepoch.ChainIndex.Tx           (ChainIndexTx (..))
import qualified Zerepoch.ChainIndex.Tx           as ChainIndex.Tx
import           Zerepoch.Contract.BccAPITemp (makeTransactionBody')
import qualified Zerepoch.V1.Ledger.Api           as Api
import qualified Zerepoch.V1.Ledger.Credential    as Credential
import qualified Zerepoch.V1.Ledger.Value         as Value
import qualified ZerepochCore.Data                as Data
import qualified ZerepochTx.Prelude               as ZerepochTx

fromBccBlock :: C.BlockInMode C.BccMode -> Either FromBccError [ChainIndexTx]
fromBccBlock (C.BlockInMode (C.Block (C.BlockHeader _ _ _) txs) era) =
  traverse (fromBccTx era) txs

fromBccTx ::C.EraInMode era C.BccMode -> C.Tx era -> Either FromBccError ChainIndexTx
fromBccTx _era (C.Tx b@(C.TxBody C.TxBodyContent{..}) _keyWitnesses) = do
    txOutputs <- traverse fromBccTxOut txOuts
    pure
        ChainIndexTx
            { _citxTxId = fromBccTxId (C.getTxId b)
            , _citxValidRange = fromBccValidityRange txValidityRange
            , _citxInputs = Set.fromList $ fmap ((`P.TxIn` Nothing) . fromBccTxIn . fst) txIns
            , _citxOutputs = ChainIndex.Tx.ValidTx txOutputs -- FIXME: Check if tx is invalid
            , _citxData = mempty -- only available with a Build Tx
            , _citxRedeemers = mempty -- only available with a Build Tx
            , _citxMintingPolicies = mempty -- only available with a Build Tx
            , _citxStakeValidators = mempty -- only available with a Build Tx
            , _citxValidators = mempty -- only available with a Build Tx
            , _citxBccTx = Nothing -- FIXME: Should be SomeTx t era, but we are missing a 'C.IsBccEra era' constraint. This constraint is required in 'Ledger.Tx' for the JSON instance.
            }

toBccTxBody ::
    Maybe C.ProtocolParameters -- ^ Protocol parameters to use. Building Zerepoch transactions will fail if this is 'Nothing'
    -> C.NetworkId -- ^ Network ID
    -> P.Tx
    -> Either ToBccError (C.TxBody C.AurumEra)
toBccTxBody protocolParams networkId P.Tx{..} = do
    txIns <- traverse toBccTxInBuild $ Set.toList txInputs
    txInsCollateral <- toBccTxInsCollateral txCollateral
    txOuts <- traverse (toBccTxOut networkId) txOutputs
    txFee' <- toBccFee txFee
    txValidityRange <- toBccValidityRange txValidRange
    txMintValue <- toBccMintValue txMint txMintScripts
    first TxBodyError $ makeTransactionBody' C.TxBodyContent
        { txIns = txIns
        , txInsCollateral = txInsCollateral
        , txOuts = txOuts
        , txFee = txFee'
        , txValidityRange = txValidityRange
        , txExtraScriptData = C.BuildTxWith $ toBccExtraScriptData (Map.elems txData)
        , txMintValue = txMintValue
        , txProtocolParams = C.BuildTxWith protocolParams
        , txScriptValidity = C.TxScriptValidityNone
        -- unused:
        , txMetadata = C.TxMetadataNone
        , txAuxScripts = C.TxAuxScriptsNone
        , txExtraKeyWits = C.TxExtraKeyWitnessesNone
        , txWithdrawals = C.TxWithdrawalsNone
        , txCertificates = C.TxCertificatesNone
        , txUpdateProposal = C.TxUpdateProposalNone
        }

fromBccTxIn :: C.TxIn -> P.TxOutRef
fromBccTxIn (C.TxIn txId (C.TxIx txIx)) = P.TxOutRef (fromBccTxId txId) (toInteger txIx)

toBccTxInBuild :: P.TxIn -> Either ToBccError (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.AurumEra))
toBccTxInBuild (P.TxIn txInRef (Just txInType)) = (,) <$> toBccTxIn txInRef <*> (C.BuildTxWith <$> toBccTxInWitness txInType)
toBccTxInBuild (P.TxIn _ Nothing) = Left MissingTxInType

toBccTxIn :: P.TxOutRef -> Either ToBccError C.TxIn
toBccTxIn (P.TxOutRef txId txIx) = C.TxIn <$> toBccTxId txId <*> pure (C.TxIx (fromInteger txIx))

fromBccTxId :: C.TxId -> P.TxId
fromBccTxId txId = P.TxId $ ZerepochTx.toBuiltin $ C.serialiseToRawBytes txId

toBccTxId :: P.TxId -> Either ToBccError C.TxId
toBccTxId (P.TxId bs) =
    tag "toBccTxId"
    $ deserialiseFromRawBytes C.AsTxId $ ZerepochTx.fromBuiltin bs

fromBccTxInsCollateral :: C.TxInsCollateral era -> Set.Set P.TxIn
fromBccTxInsCollateral C.TxInsCollateralNone       = mempty
fromBccTxInsCollateral (C.TxInsCollateral _ txIns) = Set.fromList $ fmap (P.pubKeyTxIn . fromBccTxIn) txIns

toBccTxInsCollateral :: Set.Set P.TxIn -> Either ToBccError (C.TxInsCollateral C.AurumEra)
toBccTxInsCollateral = fmap (C.TxInsCollateral C.CollateralInAurumEra) . traverse (toBccTxIn . P.txInRef) . Set.toList

fromBccTxInWitness :: C.Witness C.WitCtxTxIn era -> Either FromBccError P.TxInType
fromBccTxInWitness (C.KeyWitness C.KeyWitnessForSpending) = pure P.ConsumePublicKeyAddress
fromBccTxInWitness
    (C.ScriptWitness _
        (C.ZerepochScriptWitness C.ZerepochScriptV1InAurum C.ZerepochScriptV1
            script
            (C.ScriptDatumForTxIn datum)
            redeemer
            _))
    = pure $ P.ConsumeScriptAddress
        (P.Validator $ fromBccZerepochScript script)
        (P.Redeemer $ fromBccScriptData redeemer)
        (P.Datum $ fromBccScriptData datum)
fromBccTxInWitness (C.ScriptWitness _ C.SimpleScriptWitness{}) = pure P.ConsumeSimpleScriptAddress

toBccTxInWitness :: P.TxInType -> Either ToBccError (C.Witness C.WitCtxTxIn C.AurumEra)
toBccTxInWitness P.ConsumePublicKeyAddress = pure (C.KeyWitness C.KeyWitnessForSpending)
toBccTxInWitness P.ConsumeSimpleScriptAddress = Left SimpleScriptsNotSupportedToBcc -- TODO: Better support for simple scripts
toBccTxInWitness
    (P.ConsumeScriptAddress
        (P.Validator validator)
        (P.Redeemer redeemer)
        (P.Datum datum))
    = C.ScriptWitness C.ScriptWitnessForSpending <$>
        (C.ZerepochScriptWitness C.ZerepochScriptV1InAurum C.ZerepochScriptV1
        <$> toBccZerepochScript validator
        <*> pure (C.ScriptDatumForTxIn $ toBccScriptData datum)
        <*> pure (toBccScriptData redeemer)
        <*> toBccExecutionUnits validator [Api.builtinDataToData datum, Api.builtinDataToData redeemer] -- TODO: is [datum, redeemer] correct?
        )

toBccMintWitness :: P.MintingPolicy -> Either ToBccError (C.ScriptWitness C.WitCtxMint C.AurumEra)
toBccMintWitness (P.MintingPolicy script) = C.ZerepochScriptWitness C.ZerepochScriptV1InAurum C.ZerepochScriptV1
    <$> toBccZerepochScript script
    <*> pure C.NoScriptDatumForMint
    <*> pure (C.ScriptDataNumber 0) -- TODO: redeemers not modelled yet in Zerepoch MP scripts, value is ignored
    <*> toBccExecutionUnits script [] -- TODO: is [] correct?

fromBccTxOut :: C.TxOut era -> Either FromBccError P.TxOut
fromBccTxOut (C.TxOut addr value datumHash) =
    P.TxOut
    <$> fromBccAddress addr
    <*> pure (fromBccTxOutValue value)
    <*> pure (fromBccTxOutDatumHash datumHash)

toBccTxOut :: C.NetworkId -> P.TxOut -> Either ToBccError (C.TxOut C.AurumEra)
toBccTxOut networkId (P.TxOut addr value datumHash) =
    C.TxOut <$> toBccAddress networkId addr
            <*> toBccTxOutValue value
            <*> toBccTxOutDatumHash datumHash

fromBccAddress :: C.AddressInEra era -> Either FromBccError P.Address
fromBccAddress (C.AddressInEra C.ColeAddressInAnyEra (C.ColeAddress address)) =
    Right $ P.Address zerepochCredential Nothing
    where
      zerepochCredential :: Credential.Credential
      zerepochCredential =
          Credential.PubKeyCredential
        $ P.PubKeyHash
        $ ZerepochTx.toBuiltin
        $ addrToBase58 address

fromBccAddress (C.AddressInEra _ (C.SophieAddress _ paymentCredential stakeAddressReference)) =
    P.Address (fromBccPaymentCredential (C.fromSophiePaymentCredential paymentCredential))
        <$> fromBccStakeAddressReference (C.fromSophieStakeReference stakeAddressReference)

toBccAddress :: C.NetworkId -> P.Address -> Either ToBccError (C.AddressInEra C.AurumEra)
toBccAddress networkId (P.Address addressCredential addressStakingCredential) =
    C.AddressInEra (C.SophieAddressInEra C.SophieBasedEraAurum) <$>
        (C.makeSophieAddress networkId
            <$> toBccPaymentCredential addressCredential
            <*> toBccStakeAddressReference addressStakingCredential)

fromBccPaymentCredential :: C.PaymentCredential -> Credential.Credential
fromBccPaymentCredential (C.PaymentCredentialByKey paymentKeyHash) = Credential.PubKeyCredential (fromBccPaymentKeyHash paymentKeyHash)
fromBccPaymentCredential (C.PaymentCredentialByScript scriptHash) = Credential.ScriptCredential (fromBccScriptHash scriptHash)

toBccPaymentCredential :: Credential.Credential -> Either ToBccError C.PaymentCredential
toBccPaymentCredential (Credential.PubKeyCredential pubKeyHash) = C.PaymentCredentialByKey <$> toBccPaymentKeyHash pubKeyHash
toBccPaymentCredential (Credential.ScriptCredential validatorHash) = C.PaymentCredentialByScript <$> toBccScriptHash validatorHash

fromBccPaymentKeyHash :: C.Hash C.PaymentKey -> P.PubKeyHash
fromBccPaymentKeyHash paymentKeyHash = P.PubKeyHash $ ZerepochTx.toBuiltin $ C.serialiseToRawBytes paymentKeyHash

toBccPaymentKeyHash :: P.PubKeyHash -> Either ToBccError (C.Hash C.PaymentKey)
toBccPaymentKeyHash (P.PubKeyHash bs) = tag "toBccPaymentKeyHash" $ deserialiseFromRawBytes (C.AsHash C.AsPaymentKey) $ ZerepochTx.fromBuiltin bs

fromBccScriptHash :: C.ScriptHash -> P.ValidatorHash
fromBccScriptHash scriptHash = P.ValidatorHash $ ZerepochTx.toBuiltin $ C.serialiseToRawBytes scriptHash

toBccScriptHash :: P.ValidatorHash -> Either ToBccError C.ScriptHash
toBccScriptHash (P.ValidatorHash bs) = tag "toBccScriptHash" $ deserialiseFromRawBytes C.AsScriptHash $ ZerepochTx.fromBuiltin bs

fromBccStakeAddressReference :: C.StakeAddressReference -> Either FromBccError (Maybe Credential.StakingCredential)
fromBccStakeAddressReference C.NoStakeAddress = pure Nothing
fromBccStakeAddressReference (C.StakeAddressByValue stakeCredential) =
    pure $ Just (Credential.StakingHash $ fromBccStakeCredential stakeCredential)
fromBccStakeAddressReference C.StakeAddressByPointer{} = pure Nothing

toBccStakeAddressReference :: Maybe Credential.StakingCredential -> Either ToBccError C.StakeAddressReference
toBccStakeAddressReference Nothing = pure C.NoStakeAddress
toBccStakeAddressReference (Just (Credential.StakingHash credential)) =
    C.StakeAddressByValue <$> toBccStakeCredential credential
toBccStakeAddressReference (Just Credential.StakingPtr{}) = Left StakingPointersNotSupported

fromBccStakeCredential :: C.StakeCredential -> Credential.Credential
fromBccStakeCredential (C.StakeCredentialByKey stakeKeyHash) = Credential.PubKeyCredential (fromBccStakeKeyHash stakeKeyHash)
fromBccStakeCredential (C.StakeCredentialByScript scriptHash) = Credential.ScriptCredential (fromBccScriptHash scriptHash)

toBccStakeCredential :: Credential.Credential -> Either ToBccError C.StakeCredential
toBccStakeCredential (Credential.PubKeyCredential pubKeyHash) = C.StakeCredentialByKey <$> toBccStakeKeyHash pubKeyHash
toBccStakeCredential (Credential.ScriptCredential validatorHash) = C.StakeCredentialByScript <$> toBccScriptHash validatorHash

fromBccStakeKeyHash :: C.Hash C.StakeKey -> P.PubKeyHash
fromBccStakeKeyHash stakeKeyHash = P.PubKeyHash $ ZerepochTx.toBuiltin $ C.serialiseToRawBytes stakeKeyHash

toBccStakeKeyHash :: P.PubKeyHash -> Either ToBccError (C.Hash C.StakeKey)
toBccStakeKeyHash (P.PubKeyHash bs) = tag "toBccStakeKeyHash" $ deserialiseFromRawBytes (C.AsHash C.AsStakeKey) (ZerepochTx.fromBuiltin bs)

fromBccTxOutValue :: C.TxOutValue era -> P.Value
fromBccTxOutValue (C.TxOutBccOnly _ entropic) = fromBccEntropic entropic
fromBccTxOutValue (C.TxOutValue _ value)      = fromBccValue value

toBccTxOutValue :: P.Value -> Either ToBccError (C.TxOutValue C.AurumEra)
toBccTxOutValue value = C.TxOutValue C.MultiAssetInAurumEra <$> toBccValue value

fromBccTxOutDatumHash :: C.TxOutDatumHash era -> Maybe P.DatumHash
fromBccTxOutDatumHash C.TxOutDatumHashNone   = Nothing
fromBccTxOutDatumHash (C.TxOutDatumHash _ h) = Just $ P.DatumHash $ ZerepochTx.toBuiltin (C.serialiseToRawBytes h)

toBccTxOutDatumHash :: Maybe P.DatumHash -> Either ToBccError (C.TxOutDatumHash C.AurumEra)
toBccTxOutDatumHash Nothing = pure C.TxOutDatumHashNone
toBccTxOutDatumHash (Just (P.DatumHash bs)) = C.TxOutDatumHash C.ScriptDataInAurumEra <$> tag "toBccTxOutDatumHash" (deserialiseFromRawBytes (C.AsHash C.AsScriptData) (ZerepochTx.fromBuiltin bs))

fromBccMintValue :: C.TxMintValue build era -> P.Value
fromBccMintValue C.TxMintNone              = mempty
fromBccMintValue (C.TxMintValue _ value _) = fromBccValue value

toBccMintValue :: P.Value -> Set.Set P.MintingPolicy -> Either ToBccError (C.TxMintValue C.BuildTx C.AurumEra)
toBccMintValue value mps =
    C.TxMintValue C.MultiAssetInAurumEra
        <$> toBccValue value
        <*> (C.BuildTxWith . Map.fromList <$> traverse (\mp -> (,) <$> (toBccPolicyId . P.mintingPolicyHash) mp <*> toBccMintWitness mp) (Set.toList mps))

fromBccValue :: C.Value -> P.Value
fromBccValue (C.valueToList -> list) = foldMap toValue list
    where
        toValue (C.BccAssetId, C.Quantity q) = Bcc.entropicValueOf q
        toValue (C.AssetId policyId assetName, C.Quantity q)
            = Value.singleton (Value.mpsSymbol $ fromBccPolicyId policyId) (fromBccAssetName assetName) q

toBccValue :: P.Value -> Either ToBccError C.Value
toBccValue = fmap C.valueFromList . traverse fromValue . Value.flattenValue
    where
        fromValue (currencySymbol, tokenName, amount)
            | currencySymbol == Bcc.bccSymbol && tokenName == Bcc.bccToken =
                pure (C.BccAssetId, C.Quantity amount)
            | otherwise =
                (,) <$> (C.AssetId <$> toBccPolicyId (Value.currencyMPSHash currencySymbol) <*> pure (toBccAssetName tokenName)) <*> pure (C.Quantity amount)

fromBccPolicyId :: C.PolicyId -> P.MintingPolicyHash
fromBccPolicyId (C.PolicyId scriptHash) = P.MintingPolicyHash $ ZerepochTx.toBuiltin (C.serialiseToRawBytes scriptHash)

toBccPolicyId :: P.MintingPolicyHash -> Either ToBccError C.PolicyId
toBccPolicyId (P.MintingPolicyHash bs) = C.PolicyId <$> tag "toBccPolicyId" (tag (show (BS.length (ZerepochTx.fromBuiltin bs)) <> " bytes") (deserialiseFromRawBytes C.AsScriptHash (ZerepochTx.fromBuiltin bs)))

fromBccAssetName :: C.AssetName -> Value.TokenName
fromBccAssetName (C.AssetName bs) = Value.TokenName $ ZerepochTx.toBuiltin bs

toBccAssetName :: Value.TokenName -> C.AssetName
toBccAssetName (Value.TokenName bs) = C.AssetName $ ZerepochTx.fromBuiltin bs

fromBccFee :: C.TxFee era -> P.Value
fromBccFee (C.TxFeeImplicit _)          = mempty
fromBccFee (C.TxFeeExplicit _ entropic) = fromBccEntropic entropic

toBccFee :: P.Value -> Either ToBccError (C.TxFee C.AurumEra)
toBccFee value = C.TxFeeExplicit C.TxFeesExplicitInAurumEra <$> toBccEntropic value

fromBccEntropic :: C.Entropic -> P.Value
fromBccEntropic (C.entropicToQuantity -> C.Quantity entropic) = Bcc.entropicValueOf entropic

toBccEntropic :: P.Value -> Either ToBccError C.Entropic
toBccEntropic value = if value == Bcc.entropicValueOf entropic then pure . C.quantityToEntropic . C.Quantity $ entropic else Left ValueNotPureBcc
    where
        Bcc.Entropic entropic = Bcc.fromValue value

fromBccValidityRange :: (C.TxValidityLowerBound era, C.TxValidityUpperBound era) -> P.SlotRange
fromBccValidityRange (l, u) = P.Interval (fromBccValidityLowerBound l) (fromBccValidityUpperBound u)

toBccValidityRange :: P.SlotRange -> Either ToBccError (C.TxValidityLowerBound C.AurumEra, C.TxValidityUpperBound C.AurumEra)
toBccValidityRange (P.Interval l u) = (,) <$> toBccValidityLowerBound l <*> toBccValidityUpperBound u

fromBccValidityLowerBound :: C.TxValidityLowerBound era -> P.LowerBound P.Slot
fromBccValidityLowerBound C.TxValidityNoLowerBound = P.LowerBound P.NegInf True
fromBccValidityLowerBound (C.TxValidityLowerBound _ slotNo) = P.LowerBound (P.Finite $ fromBccSlotNo slotNo) True

toBccValidityLowerBound :: P.LowerBound P.Slot -> Either ToBccError (C.TxValidityLowerBound C.AurumEra)
toBccValidityLowerBound (P.LowerBound P.NegInf _) = pure C.TxValidityNoLowerBound
toBccValidityLowerBound (P.LowerBound (P.Finite slotNo) closed)
    = pure . C.TxValidityLowerBound C.ValidityLowerBoundInAurumEra . toBccSlotNo $ if closed then slotNo else slotNo + 1
toBccValidityLowerBound (P.LowerBound P.PosInf _) = Left InvalidValidityRange

fromBccValidityUpperBound :: C.TxValidityUpperBound era -> P.UpperBound P.Slot
fromBccValidityUpperBound (C.TxValidityNoUpperBound _) = P.UpperBound P.PosInf True
fromBccValidityUpperBound (C.TxValidityUpperBound _ slotNo) = P.UpperBound (P.Finite $ fromBccSlotNo slotNo) False

toBccValidityUpperBound :: P.UpperBound P.Slot -> Either ToBccError (C.TxValidityUpperBound C.AurumEra)
toBccValidityUpperBound (P.UpperBound P.PosInf _) = pure $ C.TxValidityNoUpperBound C.ValidityNoUpperBoundInAurumEra
toBccValidityUpperBound (P.UpperBound (P.Finite slotNo) closed)
    = pure . C.TxValidityUpperBound C.ValidityUpperBoundInAurumEra . toBccSlotNo $ if closed then slotNo + 1 else slotNo
toBccValidityUpperBound (P.UpperBound P.NegInf _) = Left InvalidValidityRange

fromBccSlotNo :: C.SlotNo -> P.Slot
fromBccSlotNo (C.SlotNo w64) = P.Slot (toInteger w64)

toBccSlotNo :: P.Slot -> C.SlotNo
toBccSlotNo (P.Slot i) = C.SlotNo (fromInteger i)

fromBccExtraScriptData :: C.TxExtraScriptData era -> [P.Datum]
fromBccExtraScriptData C.TxExtraScriptDataNone            = []
fromBccExtraScriptData (C.TxExtraScriptData _ scriptData) = fmap (P.Datum . fromBccScriptData) scriptData

toBccExtraScriptData :: [P.Datum] -> C.TxExtraScriptData C.AurumEra
toBccExtraScriptData = C.TxExtraScriptData C.ScriptDataInAurumEra . fmap (toBccScriptData . P.getDatum)

fromBccScriptData :: C.ScriptData -> Api.BuiltinData
fromBccScriptData = Api.dataToBuiltinData . C.toZerepochData

toBccScriptData :: Api.BuiltinData -> C.ScriptData
toBccScriptData = C.fromZerepochData . Api.builtinDataToData

fromBccScriptInEra :: C.ScriptInEra era -> Maybe P.Script
fromBccScriptInEra (C.ScriptInEra C.ZerepochScriptV1InAurum (C.ZerepochScript C.ZerepochScriptV1 script)) =
    Just $ fromBccZerepochScript script
fromBccScriptInEra (C.ScriptInEra _ C.SimpleScript{}) = Nothing

toBccScriptInEra :: P.Script -> Either ToBccError (C.ScriptInEra C.AurumEra)
toBccScriptInEra script = C.ScriptInEra C.ZerepochScriptV1InAurum . C.ZerepochScript C.ZerepochScriptV1 <$> toBccZerepochScript script

fromBccZerepochScript :: C.HasTypeProxy lang => C.ZerepochScript lang -> P.Script
fromBccZerepochScript = Codec.deserialise . BSL.fromStrict . C.serialiseToRawBytes

toBccZerepochScript :: P.Script -> Either ToBccError (C.ZerepochScript C.ZerepochScriptV1)
toBccZerepochScript =
    tag "toBccZerepochScript"
    . deserialiseFromRawBytes (C.AsZerepochScript C.AsZerepochScriptV1) . BSL.toStrict . Codec.serialise

toBccExecutionUnits :: P.Script -> [Data.Data] -> Either ToBccError C.ExecutionUnits
toBccExecutionUnits script datum = do
    cmp <- maybe (Left NoDefaultCostModelParams) Right Api.defaultCostModelParams -- TODO: Configurable cost model params
    let apiScript = BSS.toShort . BSL.toStrict $ Codec.serialise script
    case Api.evaluateScriptCounting Api.Quiet cmp apiScript datum of
        (_, Left err) -> Left $ EvaluationError err
        (_, Right (Api.ExBudget (Api.ExCPU cpu) (Api.ExMemory memory))) ->
            pure $ C.ExecutionUnits (fromIntegral cpu) (fromIntegral memory)

deserialiseFromRawBytes :: C.SerialiseAsRawBytes t => C.AsType t -> ByteString -> Either ToBccError t
deserialiseFromRawBytes asType = maybe (Left DeserialisationError) Right . C.deserialiseFromRawBytes asType

tag :: String -> Either ToBccError t -> Either ToBccError t
tag s = first (Tag s)

data FromBccError
    = SimpleScriptsNotSupported
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToObject)

instance Pretty FromBccError where
    pretty SimpleScriptsNotSupported        = "Simple scripts are not supported"
    -- pretty StakeAddressPointersNotSupported = "Stake address pointers are not supported"

data ToBccError
    = EvaluationError Api.EvaluationError
    | TxBodyError C.TxBodyError
    | DeserialisationError
    | InvalidValidityRange
    | ValueNotPureBcc
    | NoDefaultCostModelParams
    | StakingPointersNotSupported
    | SimpleScriptsNotSupportedToBcc
    | MissingTxInType
    | Tag String ToBccError

instance Pretty ToBccError where
    pretty (EvaluationError err)              = "EvaluationError" <> colon <+> pretty err
    pretty (TxBodyError err)                  = "TxBodyError" <> colon <+> pretty (C.displayError err)
    pretty DeserialisationError               = "ByteString deserialisation failed"
    pretty InvalidValidityRange               = "Invalid validity range"
    pretty ValueNotPureBcc                    = "Fee values should only contain Bcc"
    pretty NoDefaultCostModelParams           = "Extracting default cost model failed"
    pretty StakingPointersNotSupported        = "Staking pointers are not supported"
    pretty SimpleScriptsNotSupportedToBcc = "Simple scripts are not supported"
    pretty MissingTxInType                    = "Missing TxInType"
    pretty (Tag t err)                        = pretty t <> colon <+> pretty err
