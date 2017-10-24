module Dfinity.NetworkMonitor.Client (connect, send, Client) where

import Control.Concurrent (forkIO)
import Control.Concurrent.BoundedChan
import Control.Monad
import Data.Binary
import Data.ByteString.Lazy
import Network

import Dfinity.NetworkMonitor.Types

-- the maximum number of outstanding events
chanSize :: Int
chanSize = 1000

newtype Client = Client (BoundedChan Event)

connect :: HostName -> PortNumber -> IO Client
connect addr port = withSocketsDo $ do
  ch <- newBoundedChan chanSize
  handle <- connectTo addr (PortNumber port)
  void $ forkIO $ forever $ do
    ev <- readChan ch 
    hPut handle $ encode ev
  return $ Client ch

send :: Event -> Client -> IO ()
send ev (Client ch) = void $ tryWriteChan ch ev 