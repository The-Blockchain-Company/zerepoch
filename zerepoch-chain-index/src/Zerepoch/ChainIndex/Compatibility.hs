module Zerepoch.ChainIndex.Compatibility where

import           Bcc.Api                (Block (..), BlockHeader (..), BlockInMode (..), BlockNo (..), BccMode,
                                             ChainPoint (..), ChainTip (..), Hash, SlotNo (..), serialiseToRawBytes)
import           Ledger                     (BlockId (..), Slot (..))
import           Zerepoch.ChainIndex.Tx       (ChainIndexTx (..))
import           Zerepoch.ChainIndex.Types    (BlockNumber (..), Point (..), Tip (..))
import qualified Zerepoch.Contract.BccAPI as C

fromBccTip :: ChainTip -> Tip
fromBccTip (ChainTip slotNo hash blockNo) =
    Tip { tipSlot = fromBccSlot slotNo
        , tipBlockId = fromBccBlockId hash
        , tipBlockNo = fromBccBlockNo blockNo
        }
fromBccTip ChainTipAtGenesis = TipAtGenesis

fromBccPoint :: ChainPoint -> Point
fromBccPoint ChainPointAtGenesis = PointAtGenesis
fromBccPoint (ChainPoint slot hash) =
    Point { pointSlot = fromBccSlot slot
          , pointBlockId = fromBccBlockId hash
          }

tipFromBccBlock
  :: BlockInMode BccMode
  -> Tip
tipFromBccBlock (BlockInMode (Block (BlockHeader slot hash block) _) _) =
    fromBccTip $ ChainTip slot hash block

fromBccSlot :: SlotNo -> Slot
fromBccSlot (SlotNo slotNo) = Slot $ toInteger slotNo

fromBccBlockId :: Hash BlockHeader -> BlockId
fromBccBlockId hash =
    BlockId $ serialiseToRawBytes hash

fromBccBlockHeader :: BlockHeader -> Tip
fromBccBlockHeader (BlockHeader slotNo hash blockNo) =
    Tip { tipSlot = fromBccSlot slotNo
        , tipBlockId = fromBccBlockId hash
        , tipBlockNo = fromBccBlockNo blockNo
        }

fromBccBlockNo :: BlockNo -> BlockNumber
fromBccBlockNo (BlockNo blockNo) =
    fromIntegral $ toInteger blockNo

fromBccBlock
    :: BlockInMode BccMode
    -> Either C.FromBccError [ChainIndexTx]
fromBccBlock = C.fromBccBlock
