{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-

Events that we store in the database.

-}
module Zerepoch.PAB.Events
    ( PABEvent(..)
    , _UpdateContractInstanceState
    , _SubmitTx
    , _ActivateContract
    , _StopContract
    ) where

import           Control.Lens.TH            (makePrisms)
import           Data.Aeson                 (FromJSON, ToJSON, Value)
import           Data.Text.Prettyprint.Doc  (Pretty, pretty, (<+>))
import           GHC.Generics               (Generic)
import           Ledger.Tx                  (Tx, txId)
import           Zerepoch.Contract.Effects    (PABReq, PABResp)
import           Zerepoch.Contract.State      (ContractResponse)
import           Zerepoch.PAB.Webserver.Types (ContractActivationArgs)
import           Wallet.Types               (ContractInstanceId)

-- | A structure which ties together all possible event types into one parent.
data PABEvent t =
    UpdateContractInstanceState !(ContractActivationArgs t) !ContractInstanceId !(ContractResponse Value Value PABResp PABReq) -- ^ Update the state of a contract instance
    | SubmitTx !Tx -- ^ Send a transaction to the node
    | ActivateContract !(ContractActivationArgs t) !ContractInstanceId
    | StopContract !ContractInstanceId
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makePrisms ''PABEvent

instance Pretty t => Pretty (PABEvent t) where
    pretty = \case
        UpdateContractInstanceState t i _ -> "Update state:" <+> pretty t <+> pretty i
        SubmitTx t                        -> "SubmitTx:" <+> pretty (txId t)
        ActivateContract _ i              -> "Start contract instance" <+> pretty i
        StopContract i                    -> "Stop contract instance" <+> pretty i
