import Test.Tasty

import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [metricsTests] 

metricsTests = testGroup "Metrics"
  [ testCase "trivial" $
      1 @=? 1
  ]