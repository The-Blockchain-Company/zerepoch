{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

-- | Servant client for PAB
module Zerepoch.PAB.Webserver.Client (
    PabClient(..)
  , InstanceClient(..)
  , pabClient
) where

import           Data.Aeson                 (FromJSON, ToJSON (..))
import qualified Data.Aeson                 as JSON
import           Data.Proxy
import           Data.Text                  (Text)
import           Zerepoch.PAB.Events.Contract
import           Zerepoch.PAB.Webserver.API
import           Zerepoch.PAB.Webserver.Types
import           Servant.API
import           Servant.Client

-- | Client for PAB. The first type-argument is contract type that is used for PAB-simulator.
data PabClient t walletId = PabClient
  { healthcheck      :: ClientM ()
      -- ^ call healthcheck method
  , fullreport       :: ClientM (FullReport t)
      -- ^ call fullreport method
  , activateContract :: ContractActivationArgs t -> ClientM ContractInstanceId
      -- ^ call activate contract method
  , instanceClient   :: Text -> InstanceClient t
      -- ^ call methods for instance client. We should turn @ContractInstanceId@ to @Text@ for the first argument.
  , getWallet        :: walletId -> ClientM [ContractInstanceClientState t]
      -- ^ get wallet
  , getInstances     :: ClientM [ContractInstanceClientState t]
      -- ^ get instance
  , getDefinitions   :: ClientM [ContractSignatureResponse t]
      -- ^ get definitions
  }

-- | Contract instance endpoints
data InstanceClient t = InstanceClient
  { getInstanceStatus    :: ClientM (ContractInstanceClientState t)
      -- ^ get instance status
  , getInstanceSchema    :: ClientM (ContractSignatureResponse t)
      -- ^ get instance schema
  , callInstanceEndpoint :: String -> JSON.Value -> ClientM ()
      -- ^ call instance endpoint
  , stopInstance         :: ClientM ()
      -- ^ call stop instance method
  }

-- | Init generic pab client
pabClient :: forall t walletId. (ToJSON t, FromJSON t, ToHttpApiData walletId) => PabClient t walletId
pabClient = PabClient{..}
  where
    (healthcheck
      :<|> fullreport
      :<|> activateContract
      :<|> toInstanceClient
      :<|> getWallet
      :<|> getInstances
      :<|> getDefinitions
      ) = client (Proxy @(API t walletId))

    instanceClient cid = InstanceClient{..}
        where
          (getInstanceStatus
            :<|> getInstanceSchema
            :<|> callInstanceEndpoint
            :<|> stopInstance
            ) = toInstanceClient cid
