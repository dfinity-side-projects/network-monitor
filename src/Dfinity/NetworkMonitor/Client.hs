{-# LANGUAGE OverloadedStrings #-}

module Dfinity.NetworkMonitor.Client (connect, send, Client) where

import           Control.Concurrent             (forkIO, threadDelay)
import           Control.Concurrent.BoundedChan
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.List
import           Network.Wreq

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
connect :: String -> IO Client
connect addr = do
  -- channel of events
  ch <- newBoundedChan batchSize

  -- Launch a thread that establishes connections with the server
  -- and sends events.
  void $ forkIO $ forever $ clientLoop addr ch

  return $ Client ch

-- The clientLoop establishes connection with the monitoring server,
-- reads events off the given channel, and send events in batches to
-- the server.  If any of these steps fails, it restarts with some
-- backoff.
clientLoop :: String -> BoundedChan Event -> IO ()
clientLoop addr ch = handle onEx $ forever $ do
  -- Send a batch every batchDelay time
  threadDelay batchDelay

  -- Read up to batchSize events
  batch <- catMaybes <$> replicateM batchSize (tryReadChan ch)

  -- Send the batch if it's not empty
  unless (null batch) $ do
    r <- post (addScheme addr) $ toJSON batch
    print r
    print (r ^. responseBody)

  where
    -- Unlike the official catMaybes, our catMaybes terminates as soon
    -- as it encounters a Nothing.
    catMaybes []          = []
    catMaybes (Nothing:_) = []
    catMaybes (Just x:ms) = x : catMaybes ms

    addScheme s =
      if "://" `isInfixOf` s then s
                             else "http://" ++ s

    onEx e = let _ = (e :: SomeException) in do
      -- TODO: log the exception
      -- TODO: exponential backoff?
      -- We want to backoff a little bit so that, for instance, if the
      -- monitoring server is down, we don't want to be trying to connect
      -- with it in a busy loop.
      putStrLn $ "Exception: " ++ show e
      threadDelay backoffDelay

send :: Client -> Event -> IO ()
send (Client ch) ev = void $ tryWriteChan ch ev
