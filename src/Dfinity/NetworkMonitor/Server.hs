{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Dfinity.NetworkMonitor.Server (mainLoop) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Lucid
import           Web.Scotty

import           Dfinity.NetworkMonitor.Metrics
import           Dfinity.NetworkMonitor.Types

port :: Int
port = 3456

mainLoop :: IO ()
mainLoop = do
  metrics <- traverse newMVar allMetrics
  ch <- newChan

  -- Process events
  void $ forkIO $ forever $ process ch metrics

  -- Launch web server
  scotty port $ do
    get "/" $ do
      let showM = fmap (toHtml . show) . readMVar
      ps <- liftIO . forM metrics $ (fmap p_ . showM)
      html . renderText . mconcat $ ps

    post "/" $ do
      -- content <- body
      -- console "Got a post!"
      -- console $ show content
      -- let batch = decode content
      -- console $ "Batch: " ++ show batch
      -- console "Updating mvars!"
      -- liftIO $ sequence_ . fmap (\mvar -> modifyMVar_ mvar (return . updateBatch batch)) $ metrics
      -- console "I'm done"
      -- return ()
      batch <- jsonData
      liftIO $ sequence_ . fmap (\mvar -> modifyMVar_ mvar (return . updateBatch batch)) $ metrics
      return ()

-- console :: MonadIO m => String -> m ()
-- console s = liftIO $ putStrLn s

-- Receive events and update metrics
process :: Chan [Event] -> [MVar Metric] -> IO ()
process ch metrics = do
  evs <- readChan ch
  sequence_ . fmap (\mvar -> modifyMVar_ mvar (return . updateBatch evs)) $ metrics
