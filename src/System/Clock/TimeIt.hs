module System.Clock.TimeIt where
import System.Clock

elapsedTime :: IO a -> IO (a, Double)
elapsedTime action = do
  startTime <- getTime Monotonic 
  r <- action
  endTime   <- getTime Monotonic 

  let result = diffTime endTime startTime
               
  return (r, result)

diffTime :: TimeSpec -> TimeSpec -> Double
diffTime end start
  = (* 1e-9)
  $ fromIntegral
  $ toNanoSecs end - toNanoSecs  start