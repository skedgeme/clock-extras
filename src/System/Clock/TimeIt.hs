{-| A couple functions that probably should be in the 'clock' package. 
    See this issue: https://github.com/corsis/clock/issues/42
-}
module System.Clock.TimeIt where
import System.Clock

-- | Time an action. Return the elasped time the result
elapsedTime :: IO a -> IO (a, Double)
elapsedTime action = do
  startTime <- getTime Monotonic 
  r <- action
  endTime   <- getTime Monotonic 

  let result = diffSeconds endTime startTime

  return (r, result)

-- | Subtract two TimeSpecs and return the result in seconds
diffSeconds :: TimeSpec -> TimeSpec -> Double
diffSeconds end start
  = (* 1e-9)
  $ fromIntegral
  $ toNanoSecs end - toNanoSecs  start