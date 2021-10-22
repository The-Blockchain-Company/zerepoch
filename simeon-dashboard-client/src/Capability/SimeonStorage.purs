module Capability.SimeonStorage
  ( class ManageSimeonStorage
  , clearAllLocalStorage
  , getWalletLibrary
  , insertIntoWalletLibrary
  , getContractNicknames
  , insertIntoContractNicknames
  , addAssets
  , getContracts
  , insertContract
  , getAllWalletRoleContracts
  , getWalletRoleContracts
  , insertWalletRoleContracts
  ) where

import Prelude
import AppM (AppM)
import Control.Monad.Except (lift, runExcept)
import Data.Array (find)
import Data.Either (hush)
import Data.Foldable (for_)
import Data.Lens (set, view)
import Data.Map (Map, insert, lookup)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple)
import Effect.Class (liftEffect)
import Foreign.Generic (decodeJSON, encodeJSON)
import Halogen (HalogenM)
import LocalStorage (Key(..), getItem, removeItem, setItem)
import Simeon.PAB (ZerepochAppId)
import Simeon.Semantics (Assets, SimeonData, SimeonParams, TransactionInput)
import WalletData.Lenses (_assets, _pubKeyHash, _walletInfo, _walletNickname)
import WalletData.Types (PubKeyHash, WalletDetails, WalletLibrary)

walletLibraryLocalStorageKey :: Key
walletLibraryLocalStorageKey = Key "walletLibrary"

contractNicknamesLocalStorageKey :: Key
contractNicknamesLocalStorageKey = Key "contractNicknames"

contractsLocalStorageKey :: Key
contractsLocalStorageKey = Key "walletContracts"

walletRoleContractsLocalStorageKey :: Key
walletRoleContractsLocalStorageKey = Key "walletRoleContracts"

class
  Monad m <= ManageSimeonStorage m where
  clearAllLocalStorage :: m Unit
  -- wallet library
  getWalletLibrary :: m WalletLibrary
  insertIntoWalletLibrary :: WalletDetails -> m Unit
  -- contract nicknames
  getContractNicknames :: m (Map ZerepochAppId String)
  insertIntoContractNicknames :: ZerepochAppId -> String -> m Unit
  -- temporary data that we persist until everything is working with the PAB
  addAssets :: PubKeyHash -> Assets -> m Unit
  getContracts :: m (Map SimeonParams (Tuple SimeonData (Array TransactionInput)))
  insertContract :: SimeonParams -> (Tuple SimeonData (Array TransactionInput)) -> m Unit
  getAllWalletRoleContracts :: m (Map String (Map SimeonParams SimeonData))
  getWalletRoleContracts :: String -> m (Map SimeonParams SimeonData)
  insertWalletRoleContracts :: String -> SimeonParams -> SimeonData -> m Unit

instance manageSimeonStorageAppM :: ManageSimeonStorage AppM where
  clearAllLocalStorage =
    liftEffect do
      removeItem walletLibraryLocalStorageKey
      removeItem contractNicknamesLocalStorageKey
      removeItem contractsLocalStorageKey
      removeItem walletRoleContractsLocalStorageKey
  -- wallet library
  getWalletLibrary = do
    mWalletLibraryJson <- liftEffect $ getItem walletLibraryLocalStorageKey
    pure $ fromMaybe mempty $ hush <<< runExcept <<< decodeJSON =<< mWalletLibraryJson
  insertIntoWalletLibrary walletDetails = do
    walletLibrary <- getWalletLibrary
    let
      walletNickname = view _walletNickname walletDetails

      updatedWalletLibrary = insert walletNickname walletDetails walletLibrary
    liftEffect $ setItem walletLibraryLocalStorageKey $ encodeJSON updatedWalletLibrary
  -- contract nicknames
  getContractNicknames = do
    mContractNicknamesJson <- liftEffect $ getItem contractNicknamesLocalStorageKey
    pure $ fromMaybe mempty $ hush <<< runExcept <<< decodeJSON =<< mContractNicknamesJson
  insertIntoContractNicknames zerepochAppId nickname = do
    contractNicknames <- getContractNicknames
    let
      updatedContractNicknames = insert zerepochAppId nickname contractNicknames
    liftEffect $ setItem contractNicknamesLocalStorageKey $ encodeJSON updatedContractNicknames
  -- temporary data that we persist until everything is working with the PAB
  addAssets pubKeyHash assets = do
    walletLibrary <- getWalletLibrary
    for_ (find (\details -> view (_walletInfo <<< _pubKeyHash) details == pubKeyHash) walletLibrary) \details ->
      let
        existingAssets = view _assets details

        updatedAssets = existingAssets <> assets

        updatedDetails = set _assets updatedAssets details
      in
        insertIntoWalletLibrary updatedDetails
  getContracts = do
    mContractsJson <- liftEffect $ getItem contractsLocalStorageKey
    pure $ fromMaybe mempty $ hush <<< runExcept <<< decodeJSON =<< mContractsJson
  insertContract simeonParams contractData = do
    existingContracts <- getContracts
    let
      newContracts = insert simeonParams contractData existingContracts
    void $ liftEffect $ setItem contractsLocalStorageKey $ encodeJSON newContracts
  getAllWalletRoleContracts = do
    mAllWalletRoleContracts <- liftEffect $ getItem walletRoleContractsLocalStorageKey
    pure $ fromMaybe mempty $ hush <<< runExcept <<< decodeJSON =<< mAllWalletRoleContracts
  getWalletRoleContracts walletId = do
    allWalletRoleContracts <- getAllWalletRoleContracts
    pure $ fromMaybe mempty $ lookup walletId allWalletRoleContracts
  insertWalletRoleContracts walletId simeonParams simeonData = do
    allWalletRoleContracts <- getAllWalletRoleContracts
    walletRoleContracts <- getWalletRoleContracts walletId
    let
      newWalletRoleContracts = insert simeonParams simeonData walletRoleContracts

      newAllWalletRoleContracts = insert walletId newWalletRoleContracts allWalletRoleContracts
    void $ liftEffect $ setItem walletRoleContractsLocalStorageKey $ encodeJSON newAllWalletRoleContracts

instance manageSimeonStorageHalogenM :: ManageSimeonStorage m => ManageSimeonStorage (HalogenM state action slots msg m) where
  clearAllLocalStorage = lift clearAllLocalStorage
  getWalletLibrary = lift getWalletLibrary
  insertIntoWalletLibrary = lift <<< insertIntoWalletLibrary
  getContractNicknames = lift getContractNicknames
  insertIntoContractNicknames zerepochAppId nickname = lift $ insertIntoContractNicknames zerepochAppId nickname
  addAssets walletDetails assets = lift $ addAssets walletDetails assets
  getContracts = lift getContracts
  insertContract simeonParams contractData = lift $ insertContract simeonParams contractData
  getAllWalletRoleContracts = lift getAllWalletRoleContracts
  getWalletRoleContracts = lift <<< getWalletRoleContracts
  insertWalletRoleContracts walletId simeonParams simeonData = lift $ insertWalletRoleContracts walletId simeonParams simeonData
