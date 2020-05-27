import Data.Time ( ZonedTime, todHour, localTimeOfDay
                 , zonedTimeToLocalTime, getZonedTime )

timeNow :: ZonedTime -> IO ()
timeNow now =
  case todHour ( localTimeOfDay
                 $ zonedTimeToLocalTime now )
       < 12 of
    True  -> putStrLn "It's before noon"
    False -> putStrLn "It's after noon"

data ServicePlan
  = Free
  | Monthly
  | Annual

billAmount :: (Num p) => ServicePlan -> p
billAmount plan =
  case plan of
    Free    -> 0
    Monthly -> 5
    Annual  -> billAmount Monthly * 12

writeNumber :: (Integral a) => a -> String
writeNumber i =
  case i of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    _ -> "unknown number."

main :: IO ()
main = do
  now <- getZonedTime
  timeNow now

  let
    plan :: ServicePlan
    plan = Free
  putStrLn $ "Customer owes "
          ++ show ( billAmount plan )
          ++ " dollars."

  let
    i :: Integer
    i = 2
  putStrLn $ "Write "
          ++ show        i
          ++ " as "
          ++ writeNumber i
