{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.Tx
    ( module Export
    -- * ChainIndexTxOut
    , ChainIndexTxOut(..)
    , toTxOut
    , fromTxOut
    -- ** Lenses and Prisms
    , ciTxOutAddress
    , ciTxOutValue
    , ciTxOutDatum
    , ciTxOutValidator
    , _PublicKeyChainIndexTxOut
    , _ScriptChainIndexTxOut
    , SomeBccApiTx(..)
    -- * Transactions
    , addSignature
    , pubKeyTxOut
    , scriptTxOut
    , scriptTxOut'
    , updateUtxo
    , txOutRefs
    , unspentOutputsTx
    -- * Hashing transactions
    , txId
    ) where

import qualified Bcc.Api               as C
import           Bcc.Crypto.Hash       (SHA256, digest)
import qualified Codec.CBOR.Write          as Write
import           Codec.Serialise.Class     (Serialise, encode)
import           Control.Applicative       ((<|>))
import           Control.Lens              hiding ((.=))
import           Data.Aeson                (FromJSON (parseJSON), ToJSON (toJSON), object, (.:), (.=))
import qualified Data.Aeson                as Aeson
import           Data.Aeson.Types          (Parser, parseFail, prependFailure, typeMismatch)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Proxy
import qualified Data.Set                  as Set
import           Data.Text.Prettyprint.Doc (Pretty (pretty), braces, colon, hang, nest, viaShow, vsep, (<+>))
import           GHC.Generics              (Generic)
import           Ledger.Address            (pubKeyAddress, scriptAddress)
import           Ledger.Crypto             (PrivateKey, PubKey, signTx, toPublicKey)
import           Ledger.Scripts            (datumHash)
import           Zerepoch.V1.Ledger.Api      (Credential (PubKeyCredential, ScriptCredential), Datum, DatumHash,
                                            TxId (..), Validator, ValidatorHash, Value, addressCredential, toBuiltin)
import           Zerepoch.V1.Ledger.Tx       as Export

-- | Transaction output that comes from a chain index query.
--
-- It is defined here instead of the zerepoch-chain-index because zerepoch-ledger
-- uses that datatype, and zerepoch-chain-index can't depend on zerepoch-ledger
-- because of a cyclic dependency.
--
-- This datatype was created in order to be used in
-- 'Ledger.Constraints.processConstraint', specifically with the constraints
-- 'MustSpendPubKeyOutput' and 'MustSpendScriptOutput'.
data ChainIndexTxOut =
    PublicKeyChainIndexTxOut { _ciTxOutAddress :: Address
                             , _ciTxOutValue   :: Value
                             }
  | ScriptChainIndexTxOut { _ciTxOutAddress   :: Address
                          , _ciTxOutValidator :: Either ValidatorHash Validator
                          , _ciTxOutDatum     :: Either DatumHash Datum
                          , _ciTxOutValue     :: Value
                          }
  deriving (Show, Eq, Serialise, Generic, ToJSON, FromJSON)

makeLenses ''ChainIndexTxOut
makePrisms ''ChainIndexTxOut

-- | Converts a transaction output from the chain index to the zerepoch-ledger-api
-- transaction output.
--
-- Note that converting from 'ChainIndexTxOut' to 'TxOut' and back to
-- 'ChainIndexTxOut' loses precision ('Datum' and 'Validator' are changed to 'DatumHash' and 'ValidatorHash' respectively)
toTxOut :: ChainIndexTxOut -> TxOut
toTxOut (PublicKeyChainIndexTxOut addr v)          = TxOut addr v Nothing
toTxOut (ScriptChainIndexTxOut addr _ (Left dh) v) = TxOut addr v (Just dh)
toTxOut (ScriptChainIndexTxOut addr _ (Right d) v) = TxOut addr v (Just $ datumHash d)

-- | Converts a zerepoch-ledger-api transaction output to the chain index
-- transaction output.
fromTxOut :: TxOut -> Maybe ChainIndexTxOut
fromTxOut TxOut { txOutAddress, txOutValue, txOutDatumHash } =
  case addressCredential txOutAddress of
    PubKeyCredential _ -> pure $ PublicKeyChainIndexTxOut txOutAddress txOutValue
    ScriptCredential vh ->
      txOutDatumHash >>= \dh ->
        pure $ ScriptChainIndexTxOut txOutAddress (Left vh) (Left dh) txOutValue

instance Pretty ChainIndexTxOut where
    pretty PublicKeyChainIndexTxOut {_ciTxOutAddress, _ciTxOutValue} =
                hang 2 $ vsep ["-" <+> pretty _ciTxOutValue <+> "addressed to", pretty _ciTxOutAddress]
    pretty ScriptChainIndexTxOut {_ciTxOutAddress, _ciTxOutValue} =
                hang 2 $ vsep ["-" <+> pretty _ciTxOutValue <+> "addressed to", pretty _ciTxOutAddress]

-- TODO Move to bcc-api
deriving instance Eq (C.EraInMode era mode)

-- TODO Move to bcc-api
instance FromJSON (C.EraInMode C.ColeEra C.BccMode) where
  parseJSON "ColeEraInBccMode" = pure C.ColeEraInBccMode
  parseJSON invalid =
      prependFailure "parsing 'EraInMode ColeEra BccMode' failed, "
                     (typeMismatch "ColeEraInBccMode" invalid)

-- TODO Move to bcc-api
instance FromJSON (C.EraInMode C.SophieEra C.BccMode) where
  parseJSON "SophieEraInBccMode" = pure C.SophieEraInBccMode
  parseJSON invalid =
      prependFailure "parsing 'EraInMode SophieEra BccMode' failed, "
                     (typeMismatch "SophieEraInBccMode" invalid)

-- TODO Move to bcc-api
instance FromJSON (C.EraInMode C.EvieEra C.BccMode) where
  parseJSON "EvieEraInBccMode" = pure C.EvieEraInBccMode
  parseJSON invalid =
      prependFailure "parsing 'EraInMode EvieEra BccMode' failed, "
                     (typeMismatch "EvieEraInBccMode" invalid)

-- TODO Move to bcc-api
instance FromJSON (C.EraInMode C.JenEra C.BccMode) where
  parseJSON "JenEraInBccMode" = pure C.JenEraInBccMode
  parseJSON invalid =
      prependFailure "parsing 'EraInMode JenEra BccMode' failed, "
                     (typeMismatch "JenEraInBccMode" invalid)

-- TODO Move to bcc-api
instance FromJSON (C.EraInMode C.AurumEra C.BccMode) where
  parseJSON "AurumEraInBccMode" = pure C.AurumEraInBccMode
  parseJSON invalid =
      prependFailure "parsing 'EraInMode AurumEra BccMode' failed, "
                     (typeMismatch "AurumEraInBccMode" invalid)

-- TODO Move to bcc-api
instance ToJSON (C.EraInMode era mode) where
  toJSON C.ColeEraInColeMode     = "ColeEraInColeMode"
  toJSON C.SophieEraInSophieMode = "SophieEraInSophieMode"
  toJSON C.ColeEraInBccMode   = "ColeEraInBccMode"
  toJSON C.SophieEraInBccMode = "SophieEraInBccMode"
  toJSON C.EvieEraInBccMode = "EvieEraInBccMode"
  toJSON C.JenEraInBccMode    = "JenEraInBccMode"
  toJSON C.AurumEraInBccMode  = "AurumEraInBccMode"

data SomeBccApiTx where
  SomeTx :: C.IsBccEra era => C.Tx era -> C.EraInMode era C.BccMode -> SomeBccApiTx

instance Eq SomeBccApiTx where
  (SomeTx tx1 C.ColeEraInBccMode) == (SomeTx tx2 C.ColeEraInBccMode)     = tx1 == tx2
  (SomeTx tx1 C.SophieEraInBccMode) == (SomeTx tx2 C.SophieEraInBccMode) = tx1 == tx2
  (SomeTx tx1 C.EvieEraInBccMode) == (SomeTx tx2 C.EvieEraInBccMode) = tx1 == tx2
  (SomeTx tx1 C.JenEraInBccMode) == (SomeTx tx2 C.JenEraInBccMode)       = tx1 == tx2
  (SomeTx tx1 C.AurumEraInBccMode) == (SomeTx tx2 C.AurumEraInBccMode)   = tx1 == tx2
  _ == _                                                                           = False

deriving instance Show SomeBccApiTx

instance ToJSON SomeBccApiTx where
  toJSON (SomeTx tx eraInMode) =
    object [ "tx" .= C.serialiseToTextEnvelope Nothing tx
           , "eraInMode" .= eraInMode
           ]

-- | Converting 'SomeBccApiTx' to JSON.
--
-- If the "tx" field is from an unknown era, the JSON parser will print an
-- error at runtime while parsing.
instance FromJSON SomeBccApiTx where
  parseJSON v = parseColeInBccModeTx v
            <|> parseSophieEraInBccModeTx v
            <|> parseEvieEraInBccModeTx v
            <|> parseJenEraInBccModeTx v
            <|> parseAurumEraInBccModeTx v
            <|> parseEraInBccModeFail v

parseColeInBccModeTx :: Aeson.Value -> Parser SomeBccApiTx
parseColeInBccModeTx =
  parseSomeBccTx "Failed to parse ColeEra 'tx' field from SomeBccApiTx"
                     C.AsColeTx

parseSophieEraInBccModeTx :: Aeson.Value -> Parser SomeBccApiTx
parseSophieEraInBccModeTx =
  parseSomeBccTx "Failed to parse SophieEra 'tx' field from SomeBccApiTx"
                     C.AsSophieTx

parseJenEraInBccModeTx :: Aeson.Value -> Parser SomeBccApiTx
parseJenEraInBccModeTx =
  parseSomeBccTx "Failed to parse JenEra 'tx' field from SomeBccApiTx"
                     jenEraTxAsType

parseEvieEraInBccModeTx :: Aeson.Value -> Parser SomeBccApiTx
parseEvieEraInBccModeTx =
  parseSomeBccTx "Failed to parse EvieEra 'tx' field from SomeBccApiTx"
                     evieEraTxAsType

parseAurumEraInBccModeTx :: Aeson.Value -> Parser SomeBccApiTx
parseAurumEraInBccModeTx =
  parseSomeBccTx "Failed to parse AurumEra 'tx' field from SomeBccApiTx"
                     aurumEraTxAsType

parseEraInBccModeFail :: Aeson.Value -> Parser SomeBccApiTx
parseEraInBccModeFail _ = fail "Unable to parse 'eraInMode'"

parseSomeBccTx
  :: ( FromJSON (C.EraInMode era C.BccMode)
     , C.IsBccEra era
     )
  => String
  -> C.AsType (C.Tx era)
  -> Aeson.Value
  -> Parser SomeBccApiTx
parseSomeBccTx errorMsg txAsType (Aeson.Object v) =
  SomeTx
    <$> (v .: "tx" >>= \envelope -> either (const $ parseFail errorMsg)
                                           pure
                                           $ C.deserialiseFromTextEnvelope txAsType envelope)
    <*> v .: "eraInMode"
parseSomeBccTx _ _ invalid =
    prependFailure "parsing SomeBccApiTx failed, "
      (typeMismatch "Object" invalid)

-- TODO Add the following 3 functions in 'Bcc.Api.Tx'.

jenEraTxAsType :: C.AsType (C.Tx C.JenEra)
jenEraTxAsType = C.proxyToAsType $ Proxy @(C.Tx C.JenEra)

evieEraTxAsType :: C.AsType (C.Tx C.EvieEra)
evieEraTxAsType = C.proxyToAsType $ Proxy @(C.Tx C.EvieEra)

aurumEraTxAsType :: C.AsType (C.Tx C.AurumEra)
aurumEraTxAsType = C.proxyToAsType $ Proxy @(C.Tx C.AurumEra)

instance Pretty Tx where
    pretty t@Tx{txInputs, txCollateral, txOutputs, txMint, txFee, txValidRange, txSignatures, txMintScripts, txData} =
        let lines' =
                [ hang 2 (vsep ("inputs:" : fmap pretty (Set.toList txInputs)))
                , hang 2 (vsep ("collateral inputs:" : fmap pretty (Set.toList txCollateral)))
                , hang 2 (vsep ("outputs:" : fmap pretty txOutputs))
                , "mint:" <+> pretty txMint
                , "fee:" <+> pretty txFee
                , hang 2 (vsep ("mps:": fmap pretty (Set.toList txMintScripts)))
                , hang 2 (vsep ("signatures:": fmap (pretty . fst) (Map.toList txSignatures)))
                , "validity range:" <+> viaShow txValidRange
                , hang 2 (vsep ("data:": fmap (pretty . snd) (Map.toList txData) ))
                ]
            txid = txId t
        in nest 2 $ vsep ["Tx" <+> pretty txid <> colon, braces (vsep lines')]

-- | Compute the id of a transaction.
txId :: Tx -> TxId
-- Double hash of a transaction, excluding its witnesses.
txId tx = TxId $ toBuiltin
               $ digest (Proxy @SHA256)
               $ digest (Proxy @SHA256)
               (Write.toStrictByteString $ encode $ strip tx)

-- | Update a map of unspent transaction outputs and signatures based on the inputs
--   and outputs of a transaction.
updateUtxo :: Tx -> Map TxOutRef TxOut -> Map TxOutRef TxOut
updateUtxo tx unspent = (unspent `Map.withoutKeys` spentOutputs tx) `Map.union` unspentOutputsTx tx

-- | A list of a transaction's outputs paired with a 'TxOutRef's referring to them.
txOutRefs :: Tx -> [(TxOut, TxOutRef)]
txOutRefs t = mkOut <$> zip [0..] (txOutputs t) where
    mkOut (i, o) = (o, TxOutRef (txId t) i)

-- | The unspent outputs of a transaction.
unspentOutputsTx :: Tx -> Map TxOutRef TxOut
unspentOutputsTx t = Map.fromList $ fmap f $ zip [0..] $ txOutputs t where
    f (idx, o) = (TxOutRef (txId t) idx, o)

-- | Create a transaction output locked by a validator script hash
--   with the given data script attached.
scriptTxOut' :: Value -> Address -> Datum -> TxOut
scriptTxOut' v a ds = TxOut a v (Just (datumHash ds))

-- | Create a transaction output locked by a validator script and with the given data script attached.
scriptTxOut :: Value -> Validator -> Datum -> TxOut
scriptTxOut v vs = scriptTxOut' v (scriptAddress vs)

-- | Create a transaction output locked by a public key.
pubKeyTxOut :: Value -> PubKey -> TxOut
pubKeyTxOut v pk = TxOut (pubKeyAddress pk) v Nothing

-- | Sign the transaction with a 'PrivateKey' and add the signature to the
--   transaction's list of signatures.
addSignature :: PrivateKey -> Tx -> Tx
addSignature privK tx = tx & signatures . at pubK ?~ sig where
    sig = signTx (txId tx) privK
    pubK = toPublicKey privK

