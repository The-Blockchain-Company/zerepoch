{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
-- | State machine that manages credential tokens
module Zerepoch.Contracts.Prism.StateMachine(
    IDState(..)
    , IDAction(..)
    , UserCredential(..)
    , typedValidator
    , machineClient
    , mkMachineClient
    ) where

import           Data.Aeson                        (FromJSON, ToJSON)
import           Data.Hashable                     (Hashable)
import           GHC.Generics                      (Generic)
import qualified Ledger.Constraints                as Constraints
import           Ledger.Constraints.TxConstraints  (TxConstraints)
import           Ledger.Crypto                     (PubKeyHash)
import qualified Ledger.Typed.Scripts              as Scripts
import           Ledger.Value                      (TokenName, Value)
import           Zerepoch.Contract.StateMachine      (State (..), StateMachine (..), StateMachineClient (..), Void)
import qualified Zerepoch.Contract.StateMachine      as StateMachine
import           Zerepoch.Contracts.Prism.Credential (Credential (..), CredentialAuthority (..))
import qualified Zerepoch.Contracts.Prism.Credential as Credential
import qualified ZerepochTx
import           ZerepochTx.Prelude
import qualified Prelude                           as Haskell

data IDState =
    Active -- ^ The credential is active and can be used in transactions
    | Revoked -- ^ The credential has been revoked and can't be used anymore.
    deriving stock (Generic, Haskell.Eq, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)

data IDAction =
    PresentCredential -- ^ Present the credential in a transaction
    | RevokeCredential -- ^ Revoke the credential
    deriving stock (Generic, Haskell.Eq, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)

-- | A 'Credential' issued to a user (public key address)
data UserCredential =
    UserCredential
        { ucAddress    :: PubKeyHash
        -- ^ Address of the credential holder
        , ucCredential ::  Credential
        -- ^ The credential
        , ucToken      :: Value
        -- ^ The 'Value' containing a token of the credential
        -- (this needs to be included here because 'Credential.token'
        -- is not available in on-chain code)
        } deriving stock (Haskell.Eq, Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON, Hashable)

{-# INLINABLE transition #-}
transition :: UserCredential -> State IDState -> IDAction -> Maybe (TxConstraints Void Void, State IDState)
transition UserCredential{ucAddress, ucCredential, ucToken} State{stateData=state, stateValue=currentValue} input =
    case (state, input) of
        (Active, PresentCredential) ->
            Just
                ( Constraints.mustBeSignedBy ucAddress
                , State{stateData=Active,stateValue=currentValue}
                )
        (Active, RevokeCredential) ->
            Just
                ( Constraints.mustBeSignedBy (unCredentialAuthority $ credAuthority ucCredential)
                <> Constraints.mustMintValue (inv ucToken) -- Destroy the token
                , State{stateData=Revoked, stateValue=mempty}
                )
        _ -> Nothing

{-# INLINABLE credentialStateMachine #-}
credentialStateMachine ::
  UserCredential
  -> StateMachine IDState IDAction
credentialStateMachine cd = StateMachine.mkStateMachine Nothing (transition cd) isFinal where
  isFinal Revoked = True
  isFinal _       = False

typedValidator ::
  UserCredential
  -> Scripts.TypedValidator (StateMachine IDState IDAction)
typedValidator credentialData =
    let val = $$(ZerepochTx.compile [|| validator ||]) `ZerepochTx.applyCode` ZerepochTx.liftCode credentialData
        validator d = StateMachine.mkValidator (credentialStateMachine d)
        wrap = Scripts.wrapValidator @IDState @IDAction
    in Scripts.mkTypedValidator @(StateMachine IDState IDAction) val $$(ZerepochTx.compile [|| wrap ||])

machineClient ::
    Scripts.TypedValidator (StateMachine IDState IDAction)
    -> UserCredential
    -> StateMachineClient IDState IDAction
machineClient inst credentialData =
    let machine = credentialStateMachine credentialData
    in StateMachine.mkStateMachineClient (StateMachine.StateMachineInstance machine inst)

mkMachineClient :: CredentialAuthority -> PubKeyHash -> TokenName -> StateMachineClient IDState IDAction
mkMachineClient authority credentialOwner tokenName =
    let credential = Credential{credAuthority=authority,credName=tokenName}
        userCredential =
            UserCredential
                { ucAddress = credentialOwner
                , ucCredential = credential
                , ucToken = Credential.token credential
                }
    in machineClient (typedValidator userCredential) userCredential

ZerepochTx.makeLift ''UserCredential
ZerepochTx.makeLift ''IDState
ZerepochTx.makeLift ''IDAction
ZerepochTx.unstableMakeIsData ''IDState
ZerepochTx.unstableMakeIsData ''IDAction
