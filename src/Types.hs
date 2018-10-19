{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types (
  Job(..),
  Jobs(..),
  Build(..),
  BuildDetails(..),
  Config(..)
) where

import Data.Aeson
import Dhall
import GHC.Generics

data Job = Job { jobName :: String, jobUrl :: String }
         deriving (Generic, Show)
instance FromJSON Job where
  parseJSON = withObject "Job" $ \o -> do
    Job <$> o .: "name" <*> o.: "url"

data Jobs = Jobs { jobs :: [Job] }
          deriving (Generic, Show)
instance FromJSON Jobs

data Build = Build { buildNumber :: Int, buildUrl :: String }
           deriving (Generic, Show)
instance FromJSON Build where
  parseJSON = withObject "Build" $ \o -> do
    b <- o .: "lastCompletedBuild"
    number <- b .: "number"
    url <- b .: "url"
    return $ Build number url

data BuildDetails = BuildDetails { duration :: Int,
                                   fullDisplayName :: String,
                                   result :: String }
                  deriving (Generic, Show)
instance FromJSON BuildDetails


data Config = Config {
    userId :: String,
    apiToken :: String,
    apiEndpoint :: String
} deriving (Generic, Show)
instance Interpret Config
