{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-orphans           #-}
module Bcc.Protocol.Socket.Type where

import           Codec.Serialise.Class                              (Serialise)
import           Control.Monad                                      (forever)
import           Control.Monad.Class.MonadST                        (MonadST)
import           Control.Monad.Class.MonadTimer
import           Crypto.Hash                                        (SHA256, hash)
import           Data.Aeson                                         (FromJSON, ToJSON)
import qualified Data.Aeson.Extras                                  as JSON
import qualified Data.ByteArray                                     as BA
import qualified Data.ByteString                                    as BS
import qualified Data.ByteString.Lazy                               as BSL
import           Data.Map                                           ((!))
import qualified Data.Text                                          as Text
import           Data.Text.Prettyprint.Doc.Extras
import           Data.Time.Units.Extra                              ()
import           Data.Void                                          (Void)

import           GHC.Generics
import           NoThunks.Class                                     (NoThunks)

import           Bcc.Api                                        (NetworkId (..))
import           Bcc.Chain.Slotting                             (EpochSlots (..))
import           Codec.Serialise                                    (DeserialiseFailure)
import qualified Codec.Serialise                                    as CBOR
import           Network.TypedProtocol.Codec
import qualified Shardagnostic.Consensus.Cole.Ledger                   as Cole
import           Shardagnostic.Consensus.Bcc.Block                  (BccBlock, CodecConfig (..))
import           Shardagnostic.Consensus.Network.NodeToClient           (ClientCodecs, clientCodecs)
import           Shardagnostic.Consensus.Node.NetworkProtocolVersion    (BlockNodeToClientVersion,
                                                                     supportedNodeToClientVersions)
import qualified Shardagnostic.Consensus.Sophie.Ledger                 as Sophie
import           Shardagnostic.Consensus.Sophie.Protocol               (StandardCrypto)
import           Shardagnostic.Network.Block                            (HeaderHash, Point, StandardHash)
import           Shardagnostic.Network.Magic                            (NetworkMagic (..))
import           Shardagnostic.Network.Mux
import           Shardagnostic.Network.NodeToClient                     (NodeToClientVersion (..),
                                                                     NodeToClientVersionData (..))
import qualified Shardagnostic.Network.Protocol.ChainSync.Codec         as ChainSync
import qualified Shardagnostic.Network.Protocol.ChainSync.Type          as ChainSync
import qualified Shardagnostic.Network.Protocol.LocalTxSubmission.Codec as TxSubmission
import qualified Shardagnostic.Network.Protocol.LocalTxSubmission.Type  as TxSubmission
import           Shardagnostic.Network.Util.ShowProxy

import           Ledger                                             (Block, OnChainTx (..), Tx (..), TxId (..))

-- | Tip of the block chain type (used by node protocols).
type Tip = Block

-- | The node protocols require a block header type.
newtype BlockId = BlockId { getBlockId :: BS.ByteString }
  deriving (Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Serialise, NoThunks)
  deriving Pretty via (PrettyShow BlockId)

instance Show BlockId where
    show = Text.unpack . JSON.encodeByteString . getBlockId

-- | A hash of the block's contents.
blockId :: Block -> BlockId
blockId = BlockId
        . BA.convert
        . hash @_ @SHA256
        . BSL.toStrict
        . CBOR.serialise

-- | Explains why our (zerepoch) data structures are hashable.
type instance HeaderHash Tx = TxId
type instance HeaderHash Block = BlockId
deriving instance StandardHash Tx

-- TODO: Is this the best place for these instances?
instance ShowProxy Char
instance ShowProxy Tx where
instance ShowProxy OnChainTx where
instance ShowProxy a => ShowProxy [a] where
  showProxy _ = "[" ++ showProxy (Proxy @a) ++ "]"

deriving instance StandardHash Block
deriving newtype instance NoThunks TxId

-- | Limits for the protocols we use.
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits {
        maximumIngressQueue = maxBound
    }

-- | Protocol versions
nodeToClientVersion :: NodeToClientVersion
nodeToClientVersion = NodeToClientV_4

-- | A temporary definition of the protocol version. This will be moved as an
-- argument to the client connection function in a future PR (the network magic
-- number matches the one in the test net created by scripts)
cfgNetworkMagic :: NetworkMagic
cfgNetworkMagic = NetworkMagic 1097911063

cfgNetworkId :: NetworkId
cfgNetworkId    = Testnet cfgNetworkMagic

nodeToClientVersionData :: NodeToClientVersionData
nodeToClientVersionData = NodeToClientVersionData { networkMagic = cfgNetworkMagic }

-- | A protocol client that will never leave the initial state.
doNothingInitiatorProtocol
  :: MonadTimer m => RunMiniProtocol 'InitiatorMode BSL.ByteString m a Void
doNothingInitiatorProtocol =
    InitiatorProtocolOnly $ MuxPeerRaw $
    const $ forever $ threadDelay 1e6

doNothingResponderProtocol
  :: MonadTimer m => RunMiniProtocol 'ResponderMode BSL.ByteString m Void a
doNothingResponderProtocol =
  ResponderProtocolOnly $ MuxPeerRaw $
  const $ forever $ threadDelay 1e6

type Offset = Integer

-- | Boilerplate codecs used for protocol serialisation.

-- | The number of epochSlots is specific to each blockchain instance. This value
-- is what the bcc main and testnet uses.
epochSlots :: EpochSlots
epochSlots = EpochSlots 432000

codecVersion :: BlockNodeToClientVersion (BccBlock StandardCrypto)
codecVersion = versionMap ! nodeToClientVersion
  where
    versionMap =
      supportedNodeToClientVersions
        (Proxy @(BccBlock StandardCrypto))

codecConfig :: CodecConfig (BccBlock StandardCrypto)
codecConfig =
  BccCodecConfig
    (Cole.ColeCodecConfig epochSlots)
    Sophie.SophieCodecConfig
    Sophie.SophieCodecConfig
    Sophie.SophieCodecConfig
    Sophie.SophieCodecConfig

nodeToClientCodecs
  :: forall m. MonadST m
  => ClientCodecs (BccBlock StandardCrypto) m
nodeToClientCodecs =
  clientCodecs codecConfig codecVersion nodeToClientVersion

-- | These codecs are currently used in the mock nodes and will
--   probably soon get removed as the mock nodes are phased out.
chainSyncCodec
  :: forall block.
     ( Serialise block
     , Serialise (HeaderHash block)
     )
  => Codec (ChainSync.ChainSync block (Point block) Tip)
           DeserialiseFailure
           IO BSL.ByteString
chainSyncCodec =
    ChainSync.codecChainSync
      CBOR.encode             CBOR.decode
      CBOR.encode             CBOR.decode
      CBOR.encode             CBOR.decode

txSubmissionCodec :: Codec (TxSubmission.LocalTxSubmission Tx String)
                           DeserialiseFailure
                           IO BSL.ByteString
txSubmissionCodec =
    TxSubmission.codecLocalTxSubmission
      CBOR.encode CBOR.decode
      CBOR.encode CBOR.decode
