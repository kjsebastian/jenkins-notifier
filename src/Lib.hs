{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
  (
  ) where

import Control.Lens
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Wreq
import Types

-- Get the latest build from Jenkins
-- Store it
-- Check its status
-- If not notified, send a notification
-- Run the above on a schedule

apiSuffix = "/api/json"

-- runApp :: Config -> IO (Maybe Jobs)
runApp = do
  runReaderT $ do
    config <- ask
    mJobs <- getJobs config
    let js = maybe ([]) jobs mJobs
    details <- forM js $ \j -> do
      build <- getLatestBuild j
      case build of
        Nothing -> error "No builds for job"
        Just b -> getBuildDetails b
    return details

opts :: Reader Config Network.Wreq.Options
opts = do
  config <- ask
  return $ defaults &
    auth ?~ basicAuth (pack $ userId config) (pack $ apiToken config) &
    manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)

getJobs :: Config -> ReaderT Config IO (Maybe Jobs)
getJobs config = withJenkinsReader $ getJenkins (apiEndpoint config) ["jobs[name,url]"]

getLatestBuild :: Job -> ReaderT Config IO (Maybe Build)
getLatestBuild job = withJenkinsReader $ getJenkins (jobUrl job) ["lastCompletedBuild[number,url]"]

getBuildDetails :: Build -> ReaderT Config IO (Maybe BuildDetails)
getBuildDetails build = withJenkinsReader $ getJenkins (buildUrl build) ["duration,fullDisplayName,result"]

withJenkinsReader :: FromJSON a =>
                     ReaderT Config IO BL.ByteString ->
                     ReaderT Config IO (Maybe a)
withJenkinsReader reader = do
  config <- ask
  res <- reader
  return $ decode res

getJenkins :: String -> [Text] -> ReaderT Config IO BL.ByteString
getJenkins url treeParams = do
    config <- ask
    let httpOpts = runReader opts config
    let localOpts = httpOpts & param "tree" .~ treeParams
    res <- liftIO $ getWith localOpts (url <> apiSuffix)
    return $ res ^. responseBody
