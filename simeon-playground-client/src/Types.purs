module Types where

import Network.RemoteData (RemoteData)
import Servant.PureScript.Ajax (AjaxError)

data WarningAnalysisError
  = WarningAnalysisAjaxError AjaxError
  | WarningAnalysisIsExtendedSimeonError

type WebData
  = RemoteData AjaxError

type WarningAnalysisData
  = RemoteData WarningAnalysisError

data SimeonError
  = SimeonError String
