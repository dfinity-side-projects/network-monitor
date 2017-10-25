module Dfinity.NetworkMonitor.Client (connect, connectFullAddr, send, Client) where

import           Control.Concurrent             (forkIO, modifyMVar_, newMVar,
                                                 swapMVar, threadDelay)
import           Control.Concurrent.BoundedChan
import           Control.Exception
import           Control.Monad
import           Data.Binary
import           Data.ByteString.Lazy           (hPut)
import           Data.List.Split                (splitOn)
import           Data.Maybe
import           Network

import           Dfinity.NetworkMonitor.Types

-- the maximum number of events in a batch
batchSize :: Int
batchSize = 1000

-- the time we wait before we send a batch of events, in microseconds
batchDelay :: Int
batchDelay = 1000000  -- 1s

backoffDelay :: Int
backoffDelay = 5000000 -- 5s

newtype Client = Client (BoundedChan Event)

-- A convenience function for connecting with a full address like
-- "localhost:3456"
connectFullAddr :: String -> IO Client
connectFullAddr addr = do
  let [ip, port] = splitOn ":" addr
  connect ip (PortNumber $ read port)

connect :: HostName -> PortID -> IO Client
connect addr port = withSocketsDo $ do
  -- channel of events
  ch <- newBoundedChan batchSize

  -- Launch a thread that establishes connections with the server
  -- and sends events.
  void $ forkIO $ forever $ clientLoop ch addr port

  return $ Client ch

-- The clientLoop establishes connection with the monitoring server,
-- reads events off the given channel, and send events in batches to
-- the server.  If any of these steps fails, it restarts with some
-- backoff.
clientLoop :: BoundedChan Event -> HostName -> PortID -> IO ()
clientLoop ch addr port = handle onEx $ do
  -- establish connection
  handle <- connectTo addr port

  -- send events in batches
  void $ forkIO $ forever $ do
    -- Send a batch every batchDelay time
    threadDelay batchDelay

    -- Read up to batchSize events
    batch <- catMaybes <$> replicateM batchSize (tryReadChan ch)

    -- Send the batch
    hPut handle $ encode batch

  where
    onEx e = do
      pure (e :: SomeException)
      -- TODO: log the exception
      -- TODO: exponential backoff?
      -- We want to backoff a little bit so that, for instance, if the
      -- monitoring server is down, we don't want to be trying to connect
      -- with it in a busy loop.
      threadDelay backoffDelay
      clientLoop ch addr port

send :: Event -> Client -> IO ()
send ev (Client ch) = void $ tryWriteChan ch ev
