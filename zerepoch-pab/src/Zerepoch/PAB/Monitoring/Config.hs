{-# LANGUAGE OverloadedStrings #-}

module Zerepoch.PAB.Monitoring.Config (
      defaultConfig
    , loadConfig
    ) where

import           Bcc.BM.Configuration       (setup)
import qualified Bcc.BM.Configuration.Model as CM
import           Bcc.BM.Data.BackendKind
import           Bcc.BM.Data.Configuration  (Endpoint (..))
import           Bcc.BM.Data.Output
import           Bcc.BM.Data.Severity

-- | A default 'CM.Configuration' that logs on 'Info' and above
--   to stdout
defaultConfig :: IO CM.Configuration
defaultConfig = do
  c <- CM.empty
  CM.setMinSeverity c Info
  CM.setSetupBackends c [ KatipBK
                        , AggregationBK
                        , MonitoringBK
                        , EKGViewBK
                        ]
  CM.setDefaultBackends c [KatipBK, AggregationBK, EKGViewBK]
  CM.setSetupScribes c [ ScribeDefinition {
                          scName = "stdout"
                        , scKind = StdoutSK
                        , scFormat = ScText
                        , scPrivacy = ScPublic
                        , scRotation = Nothing
                        , scMinSev = minBound
                        , scMaxSev = maxBound
                        }]
  CM.setDefaultScribes c ["StdoutSK::stdout"]
  CM.setEKGBindAddr c $ Just (Endpoint ("127.0.0.1", 12790))
  pure c

-- | Load a 'CM.Configuration' from a YAML file.
loadConfig :: FilePath -> IO CM.Configuration
loadConfig = setup
