{-# LANGUAGE DeriveGeneric #-}

module Dfinity.NetworkMonitor.Types (Event, Metric) where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Binary (Binary)
import GHC.Generics (Generic)

type Timestamp = Int
type Height = Int
type Node = Int
type Size = Int
type Rank = Int
type Percentage = Int

data Event =
  NewRound Node Timestamp Height |
  SendBlock Node Timestamp Height Size |
  RecvBlock Node Timestamp Height 
  deriving (Generic) 

instance Binary Event

data Metric =
  BlockSentTime (Map Height (Map Rank Timestamp)) |
  BlockPropagation (Map Height (Map Rank (Map Percentage Timestamp))) 

instance Show Metric where
  show (BlockSentTime m) = undefined
  show (BlockPropagation m) = undefined

update :: Event -> Metric -> Metric 
update (SendBlock node ts height sz) (BlockSentTime m) = undefined
update (RecvBlock node ts height) (BlockPropagation m) = undefined
update _ e = e

applyEvent :: Event -> [Metric] -> [Metric] 
applyEvent e = map (update e)