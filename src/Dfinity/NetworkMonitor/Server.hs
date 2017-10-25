{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Dfinity.NetworkMonitor.Server (mainLoop) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Binary                    (decode)
import qualified Data.ByteString                as BS
import           Data.ByteString.Lazy           (fromStrict)
import           Lucid
import           Network
import           System.IO                      (Handle)
import           Web.Scotty

import           Dfinity.NetworkMonitor.Metrics
import           Dfinity.NetworkMonitor.Types

appPort :: PortID
appPort = PortNumber 3456

webPort :: Int
webPort = 3000

mainLoop :: IO ()
mainLoop = withSocketsDo $ do
  metrics <- traverse newMVar allMetrics
  ch <- newChan

  -- Process events
  void $ forkIO $ forever $ process ch metrics

  -- Launch web server
  void $ forkIO $ scotty webPort $
    get "/" $ do
      let showM = fmap (toHtml . show) . readMVar
      ps <- liftIO . forM metrics $ (fmap p_ . showM)

      html . renderText . mconcat $ ps

  -- Listen for connections
  sock <- listenOn appPort
  void $ forever $ do
    (conn, _, _) <- accept sock
    forkIO $ handleConn conn ch

-- Receive events and update metrics
process :: Chan [Event] -> [MVar Metric] -> IO ()
process ch metrics = do
  evs <- readChan ch
  sequence_ . fmap (\mvar -> modifyMVar_ mvar (return . updateBatch evs)) $ metrics

-- Read events from a connection and forward it to a channel
handleConn :: Handle -> Chan [Event] -> IO ()
handleConn conn ch = do
  msg <- BS.hGetLine conn
  writeChan ch . decode $ fromStrict msg
