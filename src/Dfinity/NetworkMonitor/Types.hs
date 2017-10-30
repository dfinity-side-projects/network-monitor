{-# LANGUAGE DeriveGeneric #-}

module Dfinity.NetworkMonitor.Types where

import           Data.Aeson
import           Data.Binary  (Binary)
import           Data.List    (foldl')
import           GHC.Generics (Generic)

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
  { _name    :: String
  , _display :: String
  , _update  :: Event -> Metric
  , _query   :: [String] -> Maybe String
  }

instance Show Metric where
  show = _display

updateBatch :: [Event] -> Metric -> Metric
updateBatch = flip $ foldl' _update
