{-# LANGUAGE DeriveGeneric #-}

module Dfinity.NetworkMonitor.Types where

import Data.Binary (Binary)
import GHC.Generics (Generic)

-- Timestamp and Duration are both in milliseconds
type Timestamp = Int
type Duration = Int
type Height = Int
type Node = Int
type Size = Int
type Rank = Int
type Percentage = Int

data Event =
  NewRound Node Timestamp Height |
  SendBlock Node Timestamp Height Rank Size |
  RecvBlock Node Timestamp Height Rank
  deriving (Generic) 

instance Binary Event

data Metric = Metric {
  _display :: String,
  _update :: Event -> Metric
}
