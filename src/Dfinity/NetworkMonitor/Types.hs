{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs         #-}

module Dfinity.NetworkMonitor.Types where

import           Data.Aeson
import           Data.Binary  (Binary)
import           GHC.Generics (Generic)
import           Web.Scotty

-- Timestamp and Duration are both in milliseconds
type Timestamp = Int
type Duration = Int
type Height = Int
type Node = Int
type Size = Int
type Rank = Int
type Percentage = Int

data Event
  = NewRound Node Timestamp Height
  | SendBlock Node Timestamp Height Rank
  | RecvBlock Node Timestamp Height Rank
  | FinishRound Node Height Duration

  deriving (Show, Generic)

instance Binary Event

instance ToJSON Event
instance FromJSON Event

data Metric = Metric
  { mName    :: String
  , mUpdate  :: [Event] -> IO ()
  , mHandler :: ActionM ()
  }
