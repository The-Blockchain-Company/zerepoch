module Capability.Contract
  ( class ManageContract
  , activateContract
  , deactivateContract
  , getContractInstanceClientState
  , getContractInstanceCurrentState
  , getContractInstanceObservableState
  , getContractInstanceHooks
  , invokeEndpoint
  , getWalletContractInstances
  , getAllContractInstances
  , getContractDefinitions
  ) where

import Prelude
import API.Lenses (_cicCurrentState, _hooks, _observableState)
import AppM (AppM)
import Bridge (toBack, toFront)
import API.Contract as API
import Control.Monad.Except (lift, runExceptT)
import Data.Lens (view)
import Data.Maybe
import Data.RawJson (RawJson)
import Foreign.Generic (class Encode)
import Halogen (HalogenM)
import Simeon.PAB (ZerepochAppId)
import SimeonContract (SimeonContract)
import Zerepoch.Contract.Effects (ActiveEndpoint)
import Zerepoch.Contract.Resumable (Request)
import Zerepoch.PAB.Events.ContractInstanceState (PartiallyDecodedResponse)
import Zerepoch.PAB.Webserver.Types (ContractActivationArgs(..), ContractInstanceClientState, ContractSignatureResponse)
import Types (AjaxResponse)
import WalletData.Types (Wallet)

-- TODO (possibly): make `AppM` a `MonadError` and remove all the `runExceptT`s
class
  Monad m <= ManageContract m where
  activateContract :: SimeonContract -> Wallet -> m (AjaxResponse ZerepochAppId)
  deactivateContract :: ZerepochAppId -> m (AjaxResponse Unit)
  getContractInstanceClientState :: ZerepochAppId -> m (AjaxResponse (ContractInstanceClientState SimeonContract))
  getContractInstanceCurrentState :: ZerepochAppId -> m (AjaxResponse (PartiallyDecodedResponse ActiveEndpoint))
  getContractInstanceObservableState :: ZerepochAppId -> m (AjaxResponse RawJson)
  getContractInstanceHooks :: ZerepochAppId -> m (AjaxResponse (Array (Request ActiveEndpoint)))
  invokeEndpoint :: forall d. Encode d => ZerepochAppId -> String -> d -> m (AjaxResponse Unit)
  getWalletContractInstances :: Wallet -> m (AjaxResponse (Array (ContractInstanceClientState SimeonContract)))
  getAllContractInstances :: m (AjaxResponse (Array (ContractInstanceClientState SimeonContract)))
  getContractDefinitions :: m (AjaxResponse (Array (ContractSignatureResponse SimeonContract)))

instance monadContractAppM :: ManageContract AppM where
  activateContract contractActivationId wallet = map toFront $ runExceptT $ API.activateContract $ ContractActivationArgs { caID: contractActivationId, caWallet: Just (toBack wallet) }
  deactivateContract zerepochAppId = runExceptT $ API.deactivateContract $ toBack zerepochAppId
  getContractInstanceClientState zerepochAppId = runExceptT $ API.getContractInstanceClientState $ toBack zerepochAppId
  getContractInstanceCurrentState zerepochAppId = do
    clientState <- getContractInstanceClientState zerepochAppId
    pure $ map (view _cicCurrentState) clientState
  getContractInstanceObservableState zerepochAppId = do
    currentState <- getContractInstanceCurrentState zerepochAppId
    pure $ map (view _observableState) currentState
  getContractInstanceHooks zerepochAppId = do
    currentState <- getContractInstanceCurrentState zerepochAppId
    pure $ map (view _hooks) currentState
  invokeEndpoint zerepochAppId endpoint payload = runExceptT $ API.invokeEndpoint (toBack zerepochAppId) endpoint payload
  getWalletContractInstances wallet = runExceptT $ API.getWalletContractInstances $ toBack wallet
  getAllContractInstances = runExceptT API.getAllContractInstances
  getContractDefinitions = runExceptT API.getContractDefinitions

instance monadContractHalogenM :: ManageContract m => ManageContract (HalogenM state action slots msg m) where
  activateContract contractActivationId wallet = lift $ activateContract contractActivationId wallet
  deactivateContract = lift <<< deactivateContract
  getContractInstanceClientState = lift <<< getContractInstanceClientState
  getContractInstanceCurrentState = lift <<< getContractInstanceCurrentState
  getContractInstanceObservableState = lift <<< getContractInstanceObservableState
  getContractInstanceHooks = lift <<< getContractInstanceHooks
  invokeEndpoint zerepochAppId endpointDescription payload = lift $ invokeEndpoint zerepochAppId endpointDescription payload
  getWalletContractInstances = lift <<< getWalletContractInstances
  getAllContractInstances = lift getAllContractInstances
  getContractDefinitions = lift getContractDefinitions
