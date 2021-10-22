{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Bcc.Protocol.Socket.Client where

import           Control.Concurrent
import           Control.Monad.Catch                         (catchAll)
import           Data.Time.Units                             (Second, TimeUnit, toMicroseconds)

import           Bcc.Api                                 (BlockInMode (..), BccMode, ChainPoint (..),
                                                              ChainTip (..), ConsensusModeParams (..),
                                                              LocalChainSyncClient (..), LocalNodeClientProtocols (..),
                                                              LocalNodeClientProtocolsInMode, LocalNodeConnectInfo (..),
                                                              NetworkId, connectToLocalNode)
import           Ledger.TimeSlot                             (SlotConfig, currentSlot)
import           Shardagnostic.Network.IOManager
import qualified Shardagnostic.Network.Protocol.ChainSync.Client as ChainSync

import           Bcc.Protocol.Socket.Type                hiding (Tip)
import           Ledger                                      (Slot (..))

data ChainSyncHandle event = ChainSyncHandle
    { cshCurrentSlot :: IO Slot
    , cshHandler     :: event -> Slot -> IO ()
    }

data ChainSyncEvent =
    Resume       !ChainPoint
  | RollForward  !(BlockInMode BccMode) !ChainTip
  | RollBackward !ChainPoint !ChainTip

{- | The `Slot` parameter here represents the `current` slot as computed from the
     current time. There is also the slot where the block was published, which is
     available from the `ChainSyncEvent`.

     Currently we are using this current slot everywhere, which is why I leave it
     here, as a parameter.
-}
type ChainSyncCallback = ChainSyncEvent -> Slot -> IO ()

getCurrentSlot
  :: forall block.
     ChainSyncHandle block
  -> IO Slot
getCurrentSlot = cshCurrentSlot

-- | Run the chain sync protocol to get access to the current slot number.
runChainSync'
  :: FilePath
  -> SlotConfig
  -> NetworkId
  -> [ChainPoint]
  -> IO (ChainSyncHandle ChainSyncEvent)
runChainSync' socketPath slotConfig networkId resumePoints =
  runChainSync socketPath slotConfig networkId resumePoints (\_ _ -> pure ())

runChainSync
  :: FilePath
  -> SlotConfig
  -> NetworkId
  -> [ChainPoint]
  -> ChainSyncCallback
  -> IO (ChainSyncHandle ChainSyncEvent)
runChainSync socketPath slotConfig networkId resumePoints chainSyncEventHandler = do
    let handle = ChainSyncHandle {
          cshCurrentSlot = currentSlot slotConfig,
          cshHandler = chainSyncEventHandler }

    _ <- forkIO $ withIOManager $ loop (1 :: Second)
    pure handle
    where
      localNodeConnectInfo = LocalNodeConnectInfo {
        localConsensusModeParams = BccModeParams epochSlots,
        localNodeNetworkId = networkId,
        localNodeSocketPath = socketPath }
      localNodeClientProtocols :: LocalNodeClientProtocolsInMode BccMode
      localNodeClientProtocols = LocalNodeClientProtocols {
        localChainSyncClient =
          LocalChainSyncClient $
            chainSyncClient slotConfig resumePoints chainSyncEventHandler,
        localTxSubmissionClient = Nothing,
        localStateQueryClient = Nothing }
      loop :: forall a. TimeUnit a
           => a
           -> IOManager
           -> IO ()
      loop timeout iocp = do
        catchAll
          (connectToLocalNode
             localNodeConnectInfo
             localNodeClientProtocols)
          (\_ -> do
               threadDelay (fromIntegral $ toMicroseconds timeout)
               loop timeout iocp)

-- | The client updates the application state when the protocol state changes.
chainSyncClient
  :: SlotConfig
  -> [ChainPoint]
  -> ChainSyncCallback
  -> ChainSync.ChainSyncClient (BlockInMode BccMode) ChainPoint ChainTip IO ()
chainSyncClient slotConfig [] chainSyncEventHandler =
  chainSyncClient slotConfig [ChainPointAtGenesis] chainSyncEventHandler
chainSyncClient slotConfig resumePoints chainSyncEventHandler =
    ChainSync.ChainSyncClient $ pure initialise
    where
      initialise :: ChainSync.ClientStIdle (BlockInMode BccMode) ChainPoint ChainTip IO ()
      initialise =
        ChainSync.SendMsgFindIntersect resumePoints $
          ChainSync.ClientStIntersect {
            ChainSync.recvMsgIntersectFound    =
              \chainPoint _ ->
                 ChainSync.ChainSyncClient $ do
                   slot <- currentSlot slotConfig
                   chainSyncEventHandler (Resume chainPoint) slot
                   pure requestNext,
            ChainSync.recvMsgIntersectNotFound =
              \_   -> ChainSync.ChainSyncClient $ pure requestNext
          }

      requestNext :: ChainSync.ClientStIdle (BlockInMode BccMode) ChainPoint ChainTip IO ()
      requestNext =
        ChainSync.SendMsgRequestNext
          handleNext
          (return handleNext)

      handleNext :: ChainSync.ClientStNext (BlockInMode BccMode) ChainPoint ChainTip IO ()
      handleNext =
        ChainSync.ClientStNext
        {
          ChainSync.recvMsgRollForward  = \block tip ->
            ChainSync.ChainSyncClient $ do
              slot <- currentSlot slotConfig
              chainSyncEventHandler (RollForward block tip) slot
              pure requestNext
        , ChainSync.recvMsgRollBackward = \point tip ->
            ChainSync.ChainSyncClient $ do
              slot <- currentSlot slotConfig
              chainSyncEventHandler (RollBackward point tip) slot
              pure requestNext
        }

