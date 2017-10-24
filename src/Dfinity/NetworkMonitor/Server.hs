module Dfinity.NetworkMonitor.Server (mainLoop) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Data.Binary
import Data.ByteString
import Data.ByteString.Lazy
import Network

import Dfinity.NetworkMonitor.Types
import Dfinity.NetworkMonitor.Metrics

port :: PortID
port = PortNumber 3456

mainLoop :: IO ()
mainLoop = withSocketsDo $ do 
  metrics <- traverse newMVar allMetrics
  ch <- newChan

  -- Process events 
  void $ forkIO $ forever $ process ch metrics

  -- Launch web server

  -- Listen for connections
  void $ forever $ do
    sock <- listenOn port
    forkIO $ handleSock sock ch

-- Receive events and update metrics
process :: Chan [Event] -> [MVar Metric] -> IO ()
process ch metrics = do
  ev <- readChan ch
  sequence_ . fmap (\mvar -> modifyMVar_ mvar (return . (`update` ev))) $ metrics

-- Read events from a socket and forward it to a channel
handleSock :: Socket -> Chan [Event] -> IO ()
handleSock sock ch = do
  (handle, _, _) <- accept sock
  msg <- hGetLine handle
  writeChan ch . decode $ fromStrict msg
