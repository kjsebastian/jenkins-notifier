{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( 
    ) where

import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Types
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Wreq

-- Get the latest build from Jenkins
-- Store it
-- Check its status
-- If not notified, send a notification
-- Run the above on a schedule

userId = "<redacted>"
apiToken = "<redacted>"
apiEndpoint = "<redacted>"
apiSuffix = "/api/json"


opts = defaults & auth ?~ basicAuth userId apiToken & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)

getJobs :: IO (Maybe Jobs)
getJobs = decode <$> getJenkins apiEndpoint ["jobs[name,url]"]

getLatestBuild :: Job -> IO (Maybe Build)
getLatestBuild job = decode <$> getJenkins (jobUrl job) ["lastCompletedBuild[number,url]"]

getBuildDetails :: Build -> IO (Maybe BuildDetails)
getBuildDetails build = decode <$> getJenkins (buildUrl build) ["duration,fullDisplayName,result"]

getJenkins :: String -> [Text] -> IO BL.ByteString
getJenkins url treeParams = do
    let localOpts = opts & param "tree" .~ treeParams
    result <- getWith localOpts (url <> apiSuffix)
    return $ result ^. responseBody

