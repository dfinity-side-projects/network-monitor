{-# LANGUAGE OverloadedStrings #-}

module Dfinity.NetworkMonitor.Metrics
  (allMetrics) where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import Data.List (sort)
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Web.Scotty

import           Dfinity.NetworkMonitor.Types

-- TODO: make this configurable
-- clusterSize :: Double
-- clusterSize = 4

-- Metrics

avgBlockLatency :: IO Metric
avgBlockLatency = newMVar (M.empty :: Map Height (Int, Int)) >>= \state -> let
  name = "avg-block-latency"

  update batch = modifyMVar_ state $ \s -> pure $ foldr _update s batch

  merge (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

  _update (FinishRound _ height duration) =
    M.insertWith merge height (1, duration)
  _update _ = id

  aggregate = M.toDescList . M.map (\(n, total) -> (realToFrac (total :: Int) / realToFrac (n :: Int)) :: Double)

  handler = do
    res <- liftIO (aggregate <$> readMVar state)
    latest <- param "latest" `rescue` (\_ -> pure 0)
    json $ case latest of
      0 -> res
      l -> take l res

  in pure $ Metric name update handler


blockPropagation :: IO Metric
blockPropagation = newMVar (M.empty :: Map (Height, Rank) (Maybe Timestamp, [Timestamp])) >>= \state -> let
  name = "block-propagation"

  update batch = modifyMVar_ state $ \s -> pure $ foldr _update s batch

  _update (SendBlock _ ts height rank) s =
    case M.lookup (height, rank) s of
      Nothing        -> M.insert (height, rank) (Just ts, []) s
      Just (_, recv) -> M.insert (height, rank) (Just ts, recv) s

  _update (RecvBlock _ ts height rank) s =
    case M.lookup (height, rank) s of
      Nothing           -> M.insert (height, rank) (Nothing, [ts]) s
      Just (send, recv) -> M.insert (height, rank) (send, ts:recv) s

  _update _ s = s

  aggregate :: Maybe (Maybe Timestamp, [Timestamp]) -> [Duration]
  aggregate (Just (Just send, recv')) = let
    recv = sort recv' in
    map (\r -> r - send) recv
  aggregate _ = []

  handler = do
    height <- param "height" `rescue` (\_ -> pure 0)
    rank <- param "rank" `rescue` (\_ -> pure 0)

    s <- liftIO $ readMVar state

    -- If height is not provided, use the latest block - 100.
    -- Minus 100 because the latest block may not have fully propagated yet. 
    json $ if height == 0 then let
      kvs = M.toDescList s
      ((nowHeight, _), _) = head kvs
      key = (if nowHeight < 100 then nowHeight else nowHeight - 100, rank)
      in 
      aggregate . M.lookup key $ s
    else
      aggregate . M.lookup (height, rank) $ s

  in pure $ Metric name update handler


allMetrics :: IO [Metric]
allMetrics = sequence [avgBlockLatency, blockPropagation]
