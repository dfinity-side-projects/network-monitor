import Test.Tasty
import Test.Tasty.HUnit

import Dfinity.NetworkMonitor.Types
import Dfinity.NetworkMonitor.Metrics

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [metricsTests] 

metricsTests = testGroup "Metrics" [blockSentTimeTests]

blockSentTimeTests = testGroup "BlockSentTime"
  [ testCase "simple" $ do
      let 
        node = 0
        h1 = 1
        r1 = 2
        ts1 = 3
        sz = 4
        m = updateBatch [SendBlock node ts1 h1 r1 sz] newBlockSentTime
      Just (show ts1) @=? _query m [show h1, show r1]
      let
        h2 = 5
        r2 = 6
        ts2 = 7
        m' = updateBatch [SendBlock node ts2 h2 r2 sz] m
      Just (show ts1) @=? _query m [show h1, show r1]
  , testCase "not found" $ do
      Nothing @=? _query newBlockSentTime ["1", "2"]
  , testCase "bad params" $ do
      Nothing @=? _query newBlockSentTime ["1", "2", "3"]]