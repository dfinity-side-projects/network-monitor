{-# LANGUAGE OverloadedStrings #-}

module Dfinity.NetworkMonitor.Metrics
  (allMetrics) where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import Data.List (sort)
import           Data.Ix                     (range)
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import  Data.Maybe                   (catMaybes)
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
    json . reverse $ case latest of
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

  numbersHandler s height rank =
    aggregate . M.lookup (height, rank) $ s

  percentageHandler s height rank sp =
    catMaybes . flip map (range (height, height + sp)) $ \h ->
      (calcPercentage $ M.lookup (h, rank) s) :: Maybe (Map Double Int)
    where
      calcPercentage Nothing = Nothing
      calcPercentage (Just (Nothing, _)) = Nothing
      calcPercentage (Just (Just send, recv')) = let
        recv = map (\r -> r - send) $ sort recv'
        n = realToFrac $ length recv
        percentages = [0.25, 0.5, 0.75, 0.99] in
        Just $ M.fromList $ map (\p -> (p, recv !! floor (n * p))) percentages

  handler = do
    s <- liftIO $ readMVar state

    typ <- param "type" `rescue` (\_ -> pure ("numbers" :: String))
    height <- param "height" `rescue` (\_ -> let
      kvs = M.toDescList s
      ((nowHeight, _), _) = head kvs in
      if nowHeight < 100 then pure nowHeight
                         else pure $ nowHeight - 100)
    rank <- param "rank" `rescue` (\_ -> pure 0)
    sp <- param "span" `rescue` (\_ -> pure 50)

    case typ of
      "numbers" -> json $ numbersHandler s height rank
      "percentage" -> json $ percentageHandler s height rank sp
      _ -> raise "unknown type"

  in pure $ Metric name update handler



allMetrics :: IO [Metric]
allMetrics = sequence [avgBlockLatency, blockPropagation]
