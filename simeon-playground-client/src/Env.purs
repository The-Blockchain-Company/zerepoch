module Env where

import Servant.PureScript.Settings (SPSettings_)
import Simeon (SPParams_)

-- Application enviroment configuration
type Env
  = { ajaxSettings :: SPSettings_ SPParams_
    }
