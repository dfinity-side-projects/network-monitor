{-# LANGUAGE OverloadedStrings #-}

module Dfinity.NetworkMonitor.Metrics
  (allMetrics) where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import qualified Data.Map                     as M
import           Web.Scotty

import           Dfinity.NetworkMonitor.Types

-- Metrics

avgBlockLatency :: IO Metric
avgBlockLatency = newMVar M.empty >>= \state -> let
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


allMetrics :: IO [Metric]
allMetrics = sequence [avgBlockLatency]
