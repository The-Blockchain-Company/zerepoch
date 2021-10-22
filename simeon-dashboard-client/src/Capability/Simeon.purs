module Capability.Simeon
  ( class ManageSimeon
  , createWallet
  , followContract
  , createPendingFollowerApp
  , followContractWithPendingFollowerApp
  , createContract
  , applyTransactionInput
  , redeem
  , lookupWalletInfo
  , lookupWalletDetails
  , getRoleContracts
  , getFollowerApps
  , subscribeToWallet
  , unsubscribeFromWallet
  , subscribeToZerepochApp
  , unsubscribeFromZerepochApp
  ) where

import Prelude
import API.Lenses (_cicContract, _cicCurrentState, _cicDefinition, _cicWallet, _observableState)
import Affjax (defaultRequest)
import AppM (AppM)
import Bridge (toBack, toFront)
import Capability.Contract (activateContract, getContractInstanceClientState, getContractInstanceObservableState, getWalletContractInstances, invokeEndpoint) as Contract
import Capability.Contract (class ManageContract)
import Capability.SimeonStorage (class ManageSimeonStorage, addAssets, getContracts, getWalletLibrary, getWalletRoleContracts, insertContract, insertWalletRoleContracts)
import Capability.Wallet (class ManageWallet)
import Capability.Wallet (createWallet, getWalletInfo, getWalletTotalFunds) as Wallet
import Capability.Websocket (class ManageWebsocket)
import Capability.Websocket (subscribeToContract, subscribeToWallet, unsubscribeFromContract, unsubscribeFromWallet) as Websocket
import Control.Monad.Except (ExceptT(..), except, lift, mapExceptT, runExcept, runExceptT, withExceptT)
import Control.Monad.Reader.Class (ask)
import Data.Array (filter) as Array
import Data.Array (find)
import Data.Bifunctor (lmap)
import Data.BigInteger (fromInt)
import Data.Either (Either(..))
import Data.Int (floor)
import Data.Lens (view)
import Data.Map (Map, findMin, fromFoldable, lookup, mapMaybeWithKey, singleton, toUnfoldable, values)
import Data.Map (filter) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple, snd)
import Data.Tuple.Nested ((/\))
import Data.UUID (genUUID, parseUUID, toString)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Random (random)
import Env (DataProvider(..))
import Foreign (MultipleErrors)
import Foreign.Generic (decodeJSON)
import Halogen (HalogenM, liftAff)
import MainFrame.Types (Msg)
import Simeon.Client (ContractHistory(..))
import Simeon.PAB (ZerepochAppId(..))
import Simeon.Semantics (Assets(..), Contract, SimeonData(..), SimeonParams(..), TokenName, TransactionInput(..), _rolePayoutValidatorHash, asset, emptyState)
import SimeonContract (SimeonContract(..))
import Zerepoch.PAB.Webserver.Types (ContractInstanceClientState)
import Zerepoch.V1.Ledger.Crypto (PubKeyHash) as Back
import Zerepoch.V1.Ledger.Value (TokenName) as Back
import ZerepochTx.AssocMap (Map) as Back
import Servant.PureScript.Ajax (AjaxError(..), ErrorDescription(..))
import Types (AjaxResponse, DecodedAjaxResponse)
import WalletData.Lenses (_companionAppId, _simeonAppId, _pubKey, _pubKeyHash, _wallet, _walletInfo)
import WalletData.Types (PubKeyHash(..), Wallet(..), WalletDetails, WalletInfo(..))

-- The `ManageSimeon` class provides a window on the `ManageContract`, `ManageWallet`, and
-- `ManageWebsocket` capabilities with functions specific to Simeon. Or rather, it does when the
-- `dataProvider` env variable is set to `PAB`. When it is set to `LocalStorage`, these functions
-- instead provide what is needed to mimic real PAB behaviour in the frontend.
-- TODO (possibly): make `AppM` a `MonadError` and remove all the `runExceptT`s
class
  (ManageContract m, ManageSimeonStorage m, ManageWallet m, ManageWebsocket m) <= ManageSimeon m where
  createWallet :: m (AjaxResponse WalletDetails)
  followContract :: WalletDetails -> SimeonParams -> m (DecodedAjaxResponse (Tuple ZerepochAppId ContractHistory))
  createPendingFollowerApp :: WalletDetails -> m (AjaxResponse ZerepochAppId)
  followContractWithPendingFollowerApp :: WalletDetails -> SimeonParams -> ZerepochAppId -> m (DecodedAjaxResponse (Tuple ZerepochAppId ContractHistory))
  createContract :: WalletDetails -> Map TokenName PubKeyHash -> Contract -> m (AjaxResponse Unit)
  applyTransactionInput :: WalletDetails -> SimeonParams -> TransactionInput -> m (AjaxResponse Unit)
  redeem :: WalletDetails -> SimeonParams -> TokenName -> m (AjaxResponse Unit)
  lookupWalletInfo :: ZerepochAppId -> m (AjaxResponse WalletInfo)
  lookupWalletDetails :: ZerepochAppId -> m (AjaxResponse WalletDetails)
  getRoleContracts :: WalletDetails -> m (DecodedAjaxResponse (Map SimeonParams SimeonData))
  getFollowerApps :: WalletDetails -> m (DecodedAjaxResponse (Map ZerepochAppId ContractHistory))
  subscribeToZerepochApp :: DataProvider -> ZerepochAppId -> m Unit
  subscribeToWallet :: DataProvider -> Wallet -> m Unit
  unsubscribeFromZerepochApp :: DataProvider -> ZerepochAppId -> m Unit
  unsubscribeFromWallet :: DataProvider -> Wallet -> m Unit

instance monadSimeonAppM :: ManageSimeon AppM where
  -- create a Wallet, together with a WalletCompanion and a SimeonApp, and return the WalletDetails
  createWallet = do
    { dataProvider } <- ask
    case dataProvider of
      SimeonPAB -> do
        ajaxWalletInfo <- Wallet.createWallet
        case ajaxWalletInfo of
          Left ajaxError -> pure $ Left ajaxError
          Right walletInfo -> do
            let
              wallet = view _wallet walletInfo
            ajaxCompanionAppId <- Contract.activateContract WalletCompanion wallet
            ajaxSimeonAppId <- Contract.activateContract SimeonApp wallet
            ajaxAssets <- Wallet.getWalletTotalFunds wallet
            let
              createWalletDetails companionAppId simeonAppId assets =
                { walletNickname: ""
                , companionAppId
                , simeonAppId
                , walletInfo
                , assets
                , previousCompanionAppState: Nothing
                }
            pure $ createWalletDetails <$> ajaxCompanionAppId <*> ajaxSimeonAppId <*> ajaxAssets
      LocalStorage -> do
        uuid <- liftEffect genUUID
        let
          uuidString = toString uuid

          walletInfo =
            WalletInfo
              { wallet: Wallet uuidString
              , pubKey: uuidString
              , pubKeyHash: PubKeyHash uuidString
              }

          assets = Assets $ singleton "" $ singleton "" (fromInt 1000000 * fromInt 10000)

          walletDetails =
            { walletNickname: ""
            , companionAppId: ZerepochAppId uuid
            , simeonAppId: ZerepochAppId uuid
            , walletInfo
            , assets
            , previousCompanionAppState: Nothing
            }
        pure $ Right walletDetails
  -- create a SimeonFollower app, call its "follow" endpoint with the given SimeonParams, and then
  -- return its ZerepochAppId and observable state
  followContract walletDetails simeonParams = do
    { dataProvider } <- ask
    case dataProvider of
      SimeonPAB ->
        runExceptT do
          let
            wallet = view (_walletInfo <<< _wallet) walletDetails
          followAppId <- withExceptT Left $ ExceptT $ Contract.activateContract SimeonFollower wallet
          void $ withExceptT Left $ ExceptT $ Contract.invokeEndpoint followAppId "follow" simeonParams
          observableStateJson <-
            withExceptT Left $ ExceptT $ Contract.getContractInstanceObservableState followAppId
          observableState <- mapExceptT (pure <<< lmap Right <<< unwrap) $ decodeJSON $ unwrap observableStateJson
          pure $ followAppId /\ observableState
      LocalStorage -> do
        existingContracts <- getContracts
        case lookup simeonParams existingContracts of
          Just (simeonData /\ transactionInputs) -> do
            uuid <- liftEffect genUUID
            let
              -- Note [SimeonParams]: In the PAB, the ZerepochAppId and the SimeonParams are completely independent,
              -- and you can have several follower apps (with different ZerepochAppIds) all following the same contract
              -- (identified by its SimeonParams). For the LocalStorage simlation we just have one follower app for
              -- each contract, and make its ZerepochAppId a function of the SimeonParams. I thought this would be
              -- simpler, but it turned out to lead to a complication (see note [PendingContracts] in Dashboard.State).
              -- I'm not going to change it now though, because this LocalStorage stuff is temporary anyway, and will
              -- be removed when the PAB is working fully.
              mUuid = parseUUID $ view _rolePayoutValidatorHash simeonParams

              followAppId = ZerepochAppId $ fromMaybe uuid mUuid

              observableState = ContractHistory { chParams: Just (simeonParams /\ simeonData), chHistory: transactionInputs }
            pure $ Right $ followAppId /\ observableState
          Nothing -> pure $ Left $ Left $ AjaxError { request: defaultRequest, description: NotFound }
  -- create a SimeonFollower app and return its ZerepochAppId, but don't call its "follow" endpoint
  -- (this function is used for creating "placeholder" contracts before we know the SimeonParams)
  createPendingFollowerApp walletDetails = do
    { dataProvider } <- ask
    case dataProvider of
      SimeonPAB -> do
        let
          wallet = view (_walletInfo <<< _wallet) walletDetails
        Contract.activateContract SimeonFollower wallet
      LocalStorage -> do
        uuid <- liftEffect genUUID
        pure $ Right $ ZerepochAppId uuid
  -- call the "follow" endpoint of a pending SimeonFollower app, and return its ZerepochAppId and
  -- observable state (to call this function, we must already know its ZerepochAppId, but we return
  -- it anyway because it is convenient to have this function return the same type as
  -- `followContract`)
  followContractWithPendingFollowerApp walletDetails simeonParams followerAppId = do
    { dataProvider } <- ask
    case dataProvider of
      SimeonPAB ->
        runExceptT do
          let
            wallet = view (_walletInfo <<< _wallet) walletDetails
          void $ withExceptT Left $ ExceptT $ Contract.invokeEndpoint followerAppId "follow" simeonParams
          observableStateJson <-
            withExceptT Left $ ExceptT $ Contract.getContractInstanceObservableState followerAppId
          observableState <- mapExceptT (pure <<< lmap Right <<< unwrap) $ decodeJSON $ unwrap observableStateJson
          pure $ followerAppId /\ observableState
      LocalStorage -> do
        existingContracts <- getContracts
        case lookup simeonParams existingContracts of
          Just (simeonData /\ transactionInputs) -> do
            uuid <- liftEffect genUUID
            let
              -- See note [SimeonParams] above.
              mUuid = parseUUID $ view _rolePayoutValidatorHash simeonParams

              correctedFollowerAppId = ZerepochAppId $ fromMaybe uuid mUuid

              observableState = ContractHistory { chParams: Just (simeonParams /\ simeonData), chHistory: transactionInputs }
            pure $ Right $ correctedFollowerAppId /\ observableState
          Nothing -> pure $ Left $ Left $ AjaxError { request: defaultRequest, description: NotFound }
  -- "create" a Simeon contract on the blockchain
  -- FIXME: if we want users to be able to follow contracts that they don't have roles in, we need this function
  -- to return the SimeonParams of the created contract - but this isn't currently possible in the PAB
  -- UPDATE to this FIXME: it is possible this won't be a problem, as it seems role tokens are first paid into
  -- the wallet that created the contract, and distributed to other wallets from there - but this remains to be
  -- seen when all the parts are working together as they should be...
  createContract walletDetails roles contract = do
    { dataProvider } <- ask
    case dataProvider of
      SimeonPAB ->
        let
          simeonAppId = view _simeonAppId walletDetails

          bRoles :: Back.Map Back.TokenName Back.PubKeyHash
          bRoles = toBack roles
        in
          Contract.invokeEndpoint simeonAppId "create" (bRoles /\ contract)
      LocalStorage -> do
        walletLibrary <- getWalletLibrary
        uuid <- liftEffect genUUID
        let
          simeonParams =
            SimeonParams
              { rolePayoutValidatorHash: toString uuid
              , rolesCurrency: { unCurrencySymbol: toString uuid }
              }

          simeonData =
            SimeonData
              { simeonContract: contract
              , simeonState: emptyState zero
              }
        void $ insertContract simeonParams (simeonData /\ mempty)
        void $ insertWalletRoleContracts (view (_walletInfo <<< _pubKey) walletDetails) simeonParams simeonData
        let
          unfoldableRoles :: Array (Tuple TokenName PubKeyHash)
          unfoldableRoles = toUnfoldable roles
        void
          $ for unfoldableRoles \(tokenName /\ pubKeyHash) -> do
              void $ addAssets pubKeyHash $ asset (toString uuid) tokenName (fromInt 1)
              void $ insertWalletRoleContracts (unwrap pubKeyHash) simeonParams simeonData
        pure $ Right unit
  -- "apply-inputs" to a Simeon contract on the blockchain
  applyTransactionInput walletDetails simeonParams transactionInput@(TransactionInput { interval, inputs }) = do
    { dataProvider } <- ask
    case dataProvider of
      SimeonPAB ->
        let
          simeonAppId = view _simeonAppId walletDetails
        in
          Contract.invokeEndpoint simeonAppId "apply-inputs" (simeonParams /\ Just interval /\ inputs)
      LocalStorage -> do
        existingContracts <- getContracts
        -- When we emulate these calls we add a 500ms delay so we give time to the submit button
        -- to show a loading indicator (we'll remove this once the PAB is connected)
        liftAff $ delay $ Milliseconds 500.0
        case lookup simeonParams existingContracts of
          Just (simeonData /\ transactionInputs) -> do
            void $ insertContract simeonParams (simeonData /\ (transactionInputs <> [ transactionInput ]))
            pure $ Right unit
          Nothing -> pure $ Left $ AjaxError { request: defaultRequest, description: NotFound }
  -- "redeem" payments from a Simeon contract on the blockchain
  redeem walletDetails simeonParams tokenName = do
    { dataProvider } <- ask
    case dataProvider of
      SimeonPAB ->
        let
          simeonAppId = view _simeonAppId walletDetails

          pubKeyHash = view (_walletInfo <<< _pubKeyHash) walletDetails
        in
          Contract.invokeEndpoint simeonAppId "redeem" (simeonParams /\ tokenName /\ pubKeyHash)
      LocalStorage -> pure $ Right unit
  -- get the WalletInfo of a wallet given the ZerepochAppId of its WalletCompanion
  lookupWalletInfo companionAppId = do
    { dataProvider } <- ask
    case dataProvider of
      SimeonPAB ->
        runExceptT do
          clientState <- ExceptT $ Contract.getContractInstanceClientState companionAppId
          case view _cicDefinition clientState of
            WalletCompanion -> do
              let
                wallet = toFront $ view _cicWallet clientState
              ExceptT $ Wallet.getWalletInfo wallet
            _ -> except $ Left $ AjaxError { request: defaultRequest, description: NotFound }
      LocalStorage ->
        runExceptT do
          walletDetails <- ExceptT $ lookupWalletDetails companionAppId
          pure $ view _walletInfo walletDetails
  -- get the WalletDetails of a wallet given the ZerepochAppId of its WalletCompanion
  -- note: this returns an empty walletNickname (because these are only saved locally)
  lookupWalletDetails companionAppId = do
    { dataProvider } <- ask
    case dataProvider of
      SimeonPAB ->
        runExceptT do
          clientState <- ExceptT $ Contract.getContractInstanceClientState companionAppId
          case view _cicDefinition clientState of
            WalletCompanion -> do
              let
                wallet = toFront $ view _cicWallet clientState
              walletContracts <- ExceptT $ Contract.getWalletContractInstances wallet
              walletInfo <- ExceptT $ Wallet.getWalletInfo wallet
              assets <- ExceptT $ Wallet.getWalletTotalFunds wallet
              case find (\state -> view _cicDefinition state == SimeonApp) walletContracts of
                Just simeonApp ->
                  ExceptT $ pure
                    $ Right
                        { walletNickname: mempty
                        , companionAppId
                        , simeonAppId: toFront $ view _cicContract simeonApp
                        , walletInfo
                        , assets
                        , previousCompanionAppState: Nothing
                        }
                Nothing -> except $ Left $ AjaxError { request: defaultRequest, description: NotFound }
            _ -> except $ Left $ AjaxError { request: defaultRequest, description: NotFound }
      LocalStorage -> do
        walletLibrary <- getWalletLibrary
        let
          mWalletDetails = findMin $ Map.filter (\walletDetails -> view _companionAppId walletDetails == companionAppId) walletLibrary
        case mWalletDetails of
          Just { key, value } -> pure $ Right value
          Nothing -> pure $ Left $ AjaxError { request: defaultRequest, description: NotFound }
  -- get the observable state of a wallet's WalletCompanion
  getRoleContracts walletDetails = do
    { dataProvider } <- ask
    case dataProvider of
      SimeonPAB ->
        runExceptT do
          let
            companionAppId = view _companionAppId walletDetails
          observableStateJson <- withExceptT Left $ ExceptT $ Contract.getContractInstanceObservableState companionAppId
          mapExceptT (pure <<< lmap Right <<< unwrap) $ decodeJSON $ unwrap observableStateJson
      LocalStorage -> do
        roleContracts <- getWalletRoleContracts $ view (_walletInfo <<< _pubKey) walletDetails
        pure $ Right roleContracts
  -- get all SimeonFollower apps for a given wallet
  getFollowerApps walletDetails = do
    { dataProvider } <- ask
    case dataProvider of
      SimeonPAB ->
        runExceptT do
          let
            wallet = view (_walletInfo <<< _wallet) walletDetails
          runningApps <- withExceptT Left $ ExceptT $ Contract.getWalletContractInstances wallet
          let
            followerApps = Array.filter (\cic -> view _cicDefinition cic == SimeonFollower) runningApps
          case traverse decodeFollowerAppState followerApps of
            Left decodingError -> except $ Left $ Right decodingError
            Right decodedFollowerApps -> ExceptT $ pure $ Right $ fromFoldable decodedFollowerApps
        where
        decodeFollowerAppState :: ContractInstanceClientState SimeonContract -> Either MultipleErrors (Tuple ZerepochAppId ContractHistory)
        decodeFollowerAppState contractInstanceClientState =
          let
            zerepochAppId = toFront $ view _cicContract contractInstanceClientState

            rawJson = view (_cicCurrentState <<< _observableState) contractInstanceClientState
          in
            case runExcept $ decodeJSON $ unwrap rawJson of
              Left decodingErrors -> Left decodingErrors
              Right observableState -> Right (zerepochAppId /\ observableState)
      LocalStorage -> do
        roleContracts <- getWalletRoleContracts $ view (_walletInfo <<< _pubKey) walletDetails
        allContracts <- getContracts
        let
          roleContractsToHistory :: SimeonParams -> SimeonData -> Maybe (Tuple ZerepochAppId ContractHistory)
          roleContractsToHistory simeonParams simeonData =
            let
              -- See note [SimeonParams] above.
              mUuid = parseUUID $ view _rolePayoutValidatorHash simeonParams

              mTransactionInputs = map snd $ lookup simeonParams allContracts
            in
              case mUuid, mTransactionInputs of
                Just uuid, Just transactionInputs ->
                  let
                    zerepochAppId = ZerepochAppId uuid

                    contractHistory = ContractHistory { chParams: Just $ simeonParams /\ simeonData, chHistory: transactionInputs }
                  in
                    Just $ zerepochAppId /\ contractHistory
                _, _ -> Nothing
        pure $ Right $ fromFoldable $ values $ mapMaybeWithKey roleContractsToHistory roleContracts
  subscribeToZerepochApp dataProvider zerepochAppId = Websocket.subscribeToContract $ toBack zerepochAppId
  subscribeToWallet dataProvider wallet = Websocket.subscribeToWallet $ toBack wallet
  unsubscribeFromZerepochApp dataProvider zerepochAppId = Websocket.unsubscribeFromContract $ toBack zerepochAppId
  unsubscribeFromWallet dataProvider wallet = Websocket.unsubscribeFromWallet $ toBack wallet

instance monadSimeonHalogenM :: (ManageSimeon m, ManageWebsocket m) => ManageSimeon (HalogenM state action slots Msg m) where
  createWallet = lift createWallet
  followContract walletDetails simeonParams = lift $ followContract walletDetails simeonParams
  createPendingFollowerApp = lift <<< createPendingFollowerApp
  followContractWithPendingFollowerApp walletDetails simeonParams followAppId = lift $ followContractWithPendingFollowerApp walletDetails simeonParams followAppId
  createContract walletDetails roles contract = lift $ createContract walletDetails roles contract
  applyTransactionInput walletDetails simeonParams transactionInput = lift $ applyTransactionInput walletDetails simeonParams transactionInput
  redeem walletDetails simeonParams tokenName = lift $ redeem walletDetails simeonParams tokenName
  lookupWalletInfo = lift <<< lookupWalletInfo
  lookupWalletDetails = lift <<< lookupWalletDetails
  getRoleContracts = lift <<< getRoleContracts
  getFollowerApps = lift <<< getFollowerApps
  subscribeToZerepochApp dataProvider zerepochAppId = when (dataProvider /= LocalStorage) $ Websocket.subscribeToContract $ toBack zerepochAppId
  subscribeToWallet dataProvider wallet = when (dataProvider /= LocalStorage) $ Websocket.subscribeToWallet $ toBack wallet
  unsubscribeFromZerepochApp dataProvider zerepochAppId = when (dataProvider /= LocalStorage) $ Websocket.unsubscribeFromContract $ toBack zerepochAppId
  unsubscribeFromWallet dataProvider wallet = when (dataProvider /= LocalStorage) $ Websocket.unsubscribeFromWallet $ toBack wallet
