{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE TypeApplications         #-}
-- Code temporarily copied over from bcc-api,
-- until https://github.com/The-Blockchain-Company/bcc-node/pull/2936 or something similar gets merged.
module Zerepoch.Contract.BccAPITemp (makeTransactionBody') where

import qualified Data.Map.Strict                    as Map
import qualified Data.Sequence.Strict               as Seq
import qualified Data.Set                           as Set

import           Bcc.Api
import           Bcc.Api.Sophie                hiding (toSophieTxOut)
import           Bcc.Ledger.BaseTypes           (StrictMaybe (..))
import           Bcc.Ledger.Crypto              (StandardCrypto)
import           Shardagnostic.Consensus.Sophie.Eras   (StandardAurum)

import qualified Bcc.Ledger.Core                as Ledger
import qualified Bcc.Ledger.Sophie.Constraints as Ledger

import qualified Bcc.Ledger.Aurum.Data         as Aurum
import qualified Bcc.Ledger.Aurum.Tx           as Aurum
import qualified Bcc.Ledger.Aurum.TxBody       as Aurum
import qualified Bcc.Ledger.Aurum.TxWitness    as Aurum

import qualified Bcc.Ledger.SophieMA.TxBody    as Evie

import qualified Bcc.Ledger.Keys                as Sophie
import qualified Sophie.Spec.Ledger.Tx             as Sophie
import qualified Sophie.Spec.Ledger.TxBody         as Sophie

makeTransactionBody' :: TxBodyContent BuildTx AurumEra -> Either TxBodyError (TxBody AurumEra)
makeTransactionBody'
    txbodycontent@TxBodyContent {
        txIns,
        txInsCollateral,
        txOuts,
        txFee,
        txValidityRange = (lowerBound, upperBound),
        txExtraScriptData,
        txExtraKeyWits,
        txWithdrawals,
        txCertificates,
        txMintValue,
        txScriptValidity
    } =
    return $
      SophieTxBody SophieBasedEraAurum
        (Aurum.TxBody
          (Set.fromList (map (toSophieTxIn . fst) txIns))
          (case txInsCollateral of
             TxInsCollateralNone     -> Set.empty
             TxInsCollateral _ txins -> Set.fromList (map toSophieTxIn txins))
          (Seq.fromList (map toSophieTxOut txOuts))
          (case txCertificates of
             TxCertificatesNone    -> Seq.empty
             TxCertificates _ cs _ -> Seq.fromList (map toSophieCertificate cs))
          (case txWithdrawals of
             TxWithdrawalsNone  -> Sophie.Wdrl Map.empty
             TxWithdrawals _ ws -> toSophieWithdrawal ws)
          (case txFee of
             TxFeeImplicit era'  -> case era' of {}
             TxFeeExplicit _ fee -> toSophieEntropic fee)
          (Evie.ValidityInterval {
             invalidBefore    = case lowerBound of
                                          TxValidityNoLowerBound   -> SNothing
                                          TxValidityLowerBound _ s -> SJust s,
             invalidHereafter = case upperBound of
                                          TxValidityNoUpperBound _ -> SNothing
                                          TxValidityUpperBound _ s -> SJust s
           })
          SNothing -- ignoring txUpdatePropsal in BccAPITemp
          (case txExtraKeyWits of
             TxExtraKeyWitnessesNone   -> Set.empty
             TxExtraKeyWitnesses _ khs -> Set.fromList
                                            [ Sophie.coerceKeyRole kh
                                            | PaymentKeyHash kh <- khs ])
          (case txMintValue of
             TxMintNone        -> mempty
             TxMintValue _ v _ -> toJenValue v)
          SNothing -- ignoring txProtocolParams in BccAPITemp
          SNothing -- ignoring txMetadata and txAuxScripts in BccAPITemp
          SNothing) -- TODO aurum: support optional network id in TxBodyContent
        scripts
        (TxBodyScriptData ScriptDataInAurumEra datums redeemers)
        Nothing -- ignoring txMetadata and txAuxScripts in BccAPITemp
        txScriptValidity
        -- TODO aurum: support the supplementary script data
  where
    witnesses :: [(ScriptWitnessIndex, AnyScriptWitness AurumEra)]
    witnesses = collectTxBodyScriptWitnesses txbodycontent

    scripts :: [Ledger.Script StandardAurum]
    scripts =
      [ toSophieScript (scriptWitnessScript scriptwitness)
      | (_, AnyScriptWitness scriptwitness) <- witnesses
      ]

    datums :: Aurum.TxDats StandardAurum
    datums =
      Aurum.TxDats $
        Map.fromList
          [ (Aurum.hashData d', d')
          | d <- scriptdata
          , let d' = toAurumData d
          ]

    scriptdata :: [ScriptData]
    scriptdata =
        [ d | BuildTxWith (TxExtraScriptData _ ds) <- [txExtraScriptData], d <- ds ]
     ++ [ d | (_, AnyScriptWitness
                    (ZerepochScriptWitness
                       _ _ _ (ScriptDatumForTxIn d) _ _)) <- witnesses
            ]

    redeemers :: Aurum.Redeemers StandardAurum
    redeemers =
      Aurum.Redeemers $
        Map.fromList
          [ (toAurumRdmrPtr idx, (toAurumData d, toAurumExUnits e))
          | (idx, AnyScriptWitness
                    (ZerepochScriptWitness _ _ _ _ d e)) <- witnesses
          ]

toSophieWithdrawal :: [(StakeAddress, Entropic, a)] -> Sophie.Wdrl StandardCrypto
toSophieWithdrawal withdrawals =
    Sophie.Wdrl $
      Map.fromList
        [ (toSophieStakeAddr stakeAddr, toSophieEntropic value)
        | (stakeAddr, value, _) <- withdrawals ]

toSophieTxOut :: forall era ledgerera.
                 (SophieLedgerEra era ~ ledgerera,
                  IsSophieBasedEra era, Ledger.SophieBased ledgerera)
               => TxOut era -> Ledger.TxOut ledgerera
toSophieTxOut (TxOut _ (TxOutBccOnly BccOnlyInColeEra _) _) =
    case sophieBasedEra :: SophieBasedEra era of {}

toSophieTxOut (TxOut addr (TxOutBccOnly BccOnlyInSophieEra value) _) =
    Sophie.TxOut (toSophieAddr addr) (toSophieEntropic value)

toSophieTxOut (TxOut addr (TxOutBccOnly BccOnlyInEvieEra value) _) =
    Sophie.TxOut (toSophieAddr addr) (toSophieEntropic value)

toSophieTxOut (TxOut addr (TxOutValue MultiAssetInJenEra value) _) =
    Sophie.TxOut (toSophieAddr addr) (toJenValue value)

toSophieTxOut (TxOut addr (TxOutValue MultiAssetInAurumEra value) txoutdata) =
    Aurum.TxOut (toSophieAddr addr) (toJenValue value)
                 (toAurumTxOutDataHash txoutdata)

toAurumTxOutDataHash :: TxOutDatumHash era
                      -> StrictMaybe (Aurum.DataHash StandardCrypto)
toAurumTxOutDataHash TxOutDatumHashNone                     = SNothing
toAurumTxOutDataHash (TxOutDatumHash _ (ScriptDataHash dh)) = SJust dh
