{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Dfinity.NetworkMonitor.Server (mainLoop) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import           Data.String                    (fromString)
import           Data.Text.Lazy                 (pack)
import           Lucid
import           Safe
import           Web.Scotty

import           Dfinity.NetworkMonitor.Metrics
import           Dfinity.NetworkMonitor.Types

port :: Int
port = 3456

mainLoop :: IO ()
mainLoop = allMetrics >>= \metrics -> scotty port $ do
  post "/" $ do
    batch <- jsonData
    liftIO $ forM_ metrics $ \m -> mUpdate m batch

  forM_ metrics $ \m -> get (fromString $ mName m) (mHandler m)

-- console :: MonadIO m => String -> m ()
-- console s = liftIO $ putStrLn s
