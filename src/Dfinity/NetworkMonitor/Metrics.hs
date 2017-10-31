module Dfinity.NetworkMonitor.Metrics
  (allMetrics) where

import           Control.Concurrent.MVar
import           Data.Map                     (Map)
import qualified Data.Map                     as M

import           Dfinity.NetworkMonitor.Types

-- TODO: make this configurable
clusterSize :: Int
clusterSize = 1000

-- Convenience functions

-- Insert a value into a two-level map.  The first two arguments are the
-- keys and the third argument is the value.
insertMap2 :: (Ord a, Ord b) => a -> b -> c -> Map a (Map b c) -> Map a (Map b c)
insertMap2 a b c =
  M.insertWith (\_ m' -> M.insert b c m') a (M.singleton b c)

-- Insert a value into a two-level map.  If the value already exists,
-- call the given function to update the value.
insertWithMap2 :: (Ord a, Ord b) => (c -> c) -> a -> b -> c -> Map a (Map b c) -> Map a (Map b c)
insertWithMap2 f a b c =
  M.insertWith (\_ m' -> M.insertWith (\_ oldVal -> f oldVal) b c m') a (M.singleton b c)

lookupMap2 :: (Ord a, Ord b) => a -> b -> Map a (Map b c) -> Maybe c
lookupMap2 a b m = M.lookup a m >>= \m' -> M.lookup b m'

-- Metrics

newAvgBlockLatency :: IO Metric
newAvgBlockLatency = newMVar M.empty >>= \state -> let
  name = undefined
  update = undefined
  handler = undefined
  in pure $ Metric name update handler


allMetrics :: IO [Metric]
allMetrics = sequence [newAvgBlockLatency]
