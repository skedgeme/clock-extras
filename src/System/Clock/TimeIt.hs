{-# LANGUAGE CPP, ViewPatterns #-}
{-| A couple functions that probably should be in the 'clock' package.
    See this issue: https://github.com/corsis/clock/issues/42
-}
module System.Clock.TimeIt where
import System.Clock

#if !MIN_VERSION_clock(0,7,0)
-- This code is ripped out of System.Clock 0.7.1
s2ns :: Num a => a
s2ns = 10^(9 :: Int)

toNanoSecs :: TimeSpec -> Integer
toNanoSecs   (TimeSpec  (toInteger -> s) (toInteger -> n)) = s * s2ns + n
#endif
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
