import Test.Hspec
import Control.Concurrent 
import System.Clock.TimeIt

main :: IO ()
main = hspec $ describe "timeIt" $ do
  it " is the same as threadDelay" $ do
    ((), time) <- elapsedTime $ threadDelay 100000 
    abs (time - 0.1) < 0.005 `shouldBe` True

