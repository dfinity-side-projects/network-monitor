module Dfinity.NetworkMonitor.Client (connect, send, Client) where

import           Control.Concurrent             (forkIO, modifyMVar_, newMVar,
                                                 swapMVar, threadDelay)
import           Control.Concurrent.BoundedChan
import           Control.Monad
import           Data.Binary
import           Data.ByteString.Lazy           (hPut)
import           Network

import           Dfinity.NetworkMonitor.Types

-- the maximum number of outstanding events
chanSize :: Int
chanSize = 1000

-- the time we wait before we send a batch of events, in microseconds
batchDelay :: Int
batchDelay = 1000000

newtype Client = Client (BoundedChan Event)

connect :: HostName -> PortID -> IO Client
connect addr port = withSocketsDo $ do
  -- channel of events
  ch <- newBoundedChan chanSize

  -- the current batch of event
  batchMVar <- newMVar []

  -- establish connection
  handle <- connectTo addr port

  -- aggregate events
  void $ forkIO $ forever $ do
    ev <- readChan ch
    modifyMVar_ batchMVar (\batch -> return $ ev:batch)

  -- send events in batches
  void $ forkIO $ forever $ do
    threadDelay batchDelay
    batch <- swapMVar batchMVar []
    unless (null batch) $
      hPut handle $ encode batch

  return $ Client ch

send :: Event -> Client -> IO ()
send ev (Client ch) = void $ tryWriteChan ch ev
