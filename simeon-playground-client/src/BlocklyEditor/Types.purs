module BlocklyEditor.Types where

import Prelude
import Analytics (class IsEvent, Event)
import Analytics as A
import BlocklyComponent.Types as Blockly
import BottomPanel.Types as BottomPanel
import Data.BigInteger (BigInteger)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Simeon.Extended.Metadata (MetadataHintInfo)
import Simeon.Template (IntegerTemplateType)
import Simeon.Linter (Warning)
import MetadataTab.Types (MetadataAction, showConstructor)
import StaticAnalysis.Types (AnalysisState, initAnalysisState)

data Action
  = HandleBlocklyMessage Blockly.Message
  | InitBlocklyProject String
  | SendToSimulator
  | ViewAsSimeon
  | Save
  | BottomPanelAction (BottomPanel.Action BottomPanelView Action)
  | AnalyseContract
  | AnalyseReachabilityContract
  | AnalyseContractForCloseRefund
  | MetadataAction MetadataAction
  | SetIntegerTemplateParam IntegerTemplateType String BigInteger
  | ClearAnalysisResults
  | SelectWarning Warning

defaultEvent :: String -> Event
defaultEvent s = (A.defaultEvent $ "BlocklyEditor." <> s) { category = Just "Blockly" }

instance blocklyActionIsEvent :: IsEvent Action where
  toEvent (HandleBlocklyMessage _) = Just $ defaultEvent "HandleBlocklyMessage"
  toEvent (InitBlocklyProject _) = Just $ defaultEvent "InitBlocklyProject"
  toEvent SendToSimulator = Just $ defaultEvent "SendToSimulator"
  toEvent ViewAsSimeon = Just $ defaultEvent "ViewAsSimeon"
  toEvent Save = Just $ defaultEvent "Save"
  toEvent (BottomPanelAction action) = A.toEvent action
  toEvent AnalyseContract = Just $ defaultEvent "AnalyseContract"
  toEvent AnalyseReachabilityContract = Just $ defaultEvent "AnalyseReachabilityContract"
  toEvent AnalyseContractForCloseRefund = Just $ defaultEvent "AnalyseContractForCloseRefund"
  toEvent (MetadataAction action) = Just $ (defaultEvent "MetadataAction") { label = Just $ showConstructor action }
  toEvent (SetIntegerTemplateParam _ _ _) = Just $ defaultEvent "SetIntegerTemplateParam"
  toEvent ClearAnalysisResults = Just $ defaultEvent "ClearAnalysisResults"
  toEvent (SelectWarning _) = Just $ defaultEvent "SelectWarning"

data BottomPanelView
  = StaticAnalysisView
  | BlocklyWarningsView
  | MetadataView

derive instance eqBottomPanelView :: Eq BottomPanelView

derive instance genericBottomPanelView :: Generic BottomPanelView _

instance showBottomPanelView :: Show BottomPanelView where
  show = genericShow

type State
  = { errorMessage :: Maybe String
    , simeonCode :: Maybe String
    , hasHoles :: Boolean
    , bottomPanelState :: BottomPanel.State BottomPanelView
    , metadataHintInfo :: MetadataHintInfo
    , analysisState :: AnalysisState
    , warnings :: Array Warning
    }

_errorMessage :: Lens' State (Maybe String)
_errorMessage = prop (SProxy :: SProxy "errorMessage")

_simeonCode :: Lens' State (Maybe String)
_simeonCode = prop (SProxy :: SProxy "simeonCode")

_hasHoles :: Lens' State Boolean
_hasHoles = prop (SProxy :: SProxy "hasHoles")

_bottomPanelState :: Lens' State (BottomPanel.State BottomPanelView)
_bottomPanelState = prop (SProxy :: SProxy "bottomPanelState")

_metadataHintInfo :: Lens' State MetadataHintInfo
_metadataHintInfo = prop (SProxy :: SProxy "metadataHintInfo")

_analysisState :: Lens' State AnalysisState
_analysisState = prop (SProxy :: SProxy "analysisState")

_warnings :: Lens' State (Array Warning)
_warnings = prop (SProxy :: SProxy "warnings")

initialState :: State
initialState =
  { errorMessage: Nothing
  , simeonCode: Nothing
  , hasHoles: false
  , bottomPanelState: BottomPanel.initialState MetadataView
  , metadataHintInfo: mempty
  , analysisState: initAnalysisState
  , warnings: mempty
  }
