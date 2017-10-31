{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Dfinity.NetworkMonitor.Server (mainLoop) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import           Data.Text.Lazy                 (pack)
import           Lucid
import           Safe
import           Web.Scotty

import           Dfinity.NetworkMonitor.Metrics
import           Dfinity.NetworkMonitor.Types

port :: Int
port = 3456

mainLoop :: IO ()
mainLoop = do
  metrics <- traverse newMVar allMetrics
  let
    findMetric = do
      mname <- param "metric"
      liftIO $ headMay . filter (\Metric{..} -> _name == mname)
        <$> mapM readMVar metrics
    metricNotFoundMsg =
      pack $ "Metric not found.  Available metrics are: " ++
        intercalate ", " (fmap _name allMetrics)
    invalidQueryMsg = pack "invalid query"

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
      batch <- jsonData
      liftIO $ sequence_ . fmap (\mvar -> modifyMVar_ mvar (return . updateBatch batch)) $ metrics
      return ()

    get "/:metric" $ do
      metric <- findMetric
      case metric of
        Nothing -> text metricNotFoundMsg
        Just m  -> html . pack $ show m

    -- Use regex matching so you don't have to have separate routes for
    -- different numbers of parameters.
    get "/:metric/:p1" $ do
      metric <- findMetric
      p1 <- param "p1"
      case metric of
        Nothing -> text metricNotFoundMsg
        Just m ->
          case _query m [p1] of
            Nothing -> text invalidQueryMsg
            Just r  -> html $ pack r

    get "/:metric/:p1/:p2" $ do
      metric <- findMetric
      p1 <- param "p1"
      p2 <- param "p2"
      case metric of
        Nothing -> text metricNotFoundMsg
        Just m ->
          case _query m [p1, p2] of
            Nothing -> text invalidQueryMsg
            Just r  -> html $ pack r

    get "/:metric/:p1/:p2/:p3" $ do
      metric <- findMetric
      p1 <- param "p1"
      p2 <- param "p2"
      p3 <- param "p3"
      case metric of
        Nothing -> text metricNotFoundMsg
        Just m ->
          case _query m [p1, p2, p3] of
            Nothing -> text invalidQueryMsg
            Just r  -> html $ pack r

-- console :: MonadIO m => String -> m ()
-- console s = liftIO $ putStrLn s

-- Receive events and update metrics
process :: Chan [Event] -> [MVar Metric] -> IO ()
process ch metrics = do
  evs <- readChan ch
  sequence_ . fmap (\mvar -> modifyMVar_ mvar (return . updateBatch evs)) $ metrics
