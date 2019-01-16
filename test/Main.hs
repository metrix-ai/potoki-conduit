module Main where

import Prelude hiding (choose)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Property as QuickCheck
import qualified Potoki.Conduit.MonadIO as MonadIO
import qualified Potoki.Consume as Consume
import qualified Potoki.IO as IO
import qualified Conduit


main =
  defaultMain $
  testGroup "All" $
  [
    testProperty "List restoration" $ \ (list :: [Int]) -> unsafePerformIO $ do
      restoredList <- MonadIO.consumeConduit (Conduit.yieldMany list) Consume.list
      return (list === restoredList)
  ]
