module Dfinity.NetworkMonitor.Metrics
  ( allMetrics
  , newBlockSentTime
  , newBlockPropagation) where

import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Text.Read

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

-- BlockSentTime records the time that each block is sent, indexed by
-- their height and rank.
newBlockSentTime :: Metric
newBlockSentTime = blockSentTime M.empty

blockSentTime :: Map Height (Map Rank Timestamp) -> Metric
blockSentTime state = Metric name display update query
  where
    name = "block-sent-time"

    display = show state

    update (SendBlock _ ts height rank) =
      blockSentTime $ insertMap2 height rank ts state
    update _ = blockSentTime state

    query [height, rank] = do
      h <- readMaybe height
      r <- readMaybe rank
      res <- lookupMap2 h r state
      return $ show res
    query _ = Nothing

-- BlockPropagation records the time it took for each block to reach
-- each node in the network, and displays the time it took for each
-- block to reach certain percentages of the network.
newBlockPropagation :: Metric
newBlockPropagation = blockPropagation $ Right M.empty

-- In the deepest level, the first element of the tuple is the time the
-- block is sent, and the second element is the list of times when the
-- block is received at different nodes.
blockPropagation :: Either (Map Percentage Duration) (Map Height (Map Rank (Maybe Timestamp, [Timestamp]))) -> Metric
blockPropagation s@(Right state) = Metric name display update query
  where
    name = "block-propagation"

    display = "still aggregating events..."

    update (SendBlock _ ts height rank) = blockPropagation . Right $
      insertWithMap2 f height rank (Just ts, []) state
      where
        f (_, recv) = (Just ts, recv)

    update (RecvBlock _ ts height rank) = blockPropagation $
      let
        postState = insertWithMap2 f height rank (Nothing, [ts]) state
        -- We have to be able to find the value since we just inserted
        -- something.
        Just (sent, recv) = lookupMap2 height rank postState in
        if length recv == clusterSize then
          Left $ compute (sent, recv)
        else
          Right postState
      where
        f (sent, recv) = (sent, ts:recv)
        compute = undefined

    update _ = blockPropagation s

    query = undefined

blockPropagation (Left _) = undefined

allMetrics :: [Metric]
allMetrics = [newBlockSentTime, newBlockPropagation]
