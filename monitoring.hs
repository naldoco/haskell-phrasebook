{-# LANGUAGE NumericUnderscores
           , TypeApplications #-}

import qualified Network.Socket as S ( SockAddr(SockAddrUnix)
                                     , Family(AF_UNIX)
                                     , SocketType(Stream)
                                     , Socket
                                     , accept, close, socket
                                     , bind, listen
                                     , defaultProtocol
                                     , maxListenQueue
                                     , connect )
-- import Network.Socket.Types ( Socket )
import Network.Socket.ByteString     ( recv, sendAll )

import Control.Exception.Safe ( bracket, mask )

import qualified Data.ByteString as BS ( ByteString, length )
import qualified Data.ByteString.Char8 ( pack, unpack )

import System.Environment     ( getArgs )
import System.Exit            ( die )
import System.Signal          ( installHandler, sigTERM )
import System.Process         ( spawnCommand, callCommand
                              , terminateProcess
                              , waitForProcess )

import Control.Concurrent       ( forkFinally, threadDelay )
import Control.Concurrent.Async ( race_ )
import Control.Concurrent.STM   ( TQueue
                                , atomically, newTQueue
                                , newTVar, readTVar
                                , writeTVar, check
                                , writeTQueue, readTQueue )

import qualified Data.Sequence as Seq ( Seq, length, take
                                      , filter, empty, (<|) )
import Data.Ratio ( (%) )

import Control.Monad ( forever, when )
import Data.Foldable ( asum, for_, find )
import Data.Maybe    ( mapMaybe )

import GHC.Real ( Ratio )

-- An event report represents the result of a single action.
data EventReport
  = Success
  | Failure
  deriving Eq

-- The system status is an overview of whether,
-- in general, actions tend to be succeeding or failing. --
data SystemStatus
  = Okay
  | Alarm
  deriving Eq

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["aggregate-reports"]  -> aggregateReportsMain
    ["send-demo-reports"]  -> sendDemoReportsMain
    ["full-demonstration"] -> fullDemonstrationMain
    _                      -> die "Invalid args"

aggregateReportsMain :: IO ()
aggregateReportsMain = do
  reportQueue <- atomically newTQueue
  alarmQueue  <- atomically newTQueue

  foldr1 race_
    [ receiveReports reportQueue
    , analyzeReports reportQueue alarmQueue
    , sendAlarms alarmQueue
    , waitForTerminationSignal
    ]

  putStrLn "The monitoring server is stopping."

waitForTerminationSignal :: IO ()
waitForTerminationSignal = do
  terminate <- atomically $ newTVar False
  installHandler sigTERM $ \_signal ->
    atomically $ writeTVar terminate True
  atomically $ readTVar terminate >>= check

---  Message format  ---

encodeReport :: EventReport -> Char
encodeReport r =
  case r of
    Failure -> '0'
    Success -> '1'

decodeReport :: Char -> Maybe EventReport
decodeReport c =
  find (\r -> encodeReport r == c)
       [Failure, Success]

---  Receiving event reports  ---

serverAddress :: S.SockAddr
serverAddress =
  S.SockAddrUnix "\0haskell-phrasebook/monitoring"

openSocket :: IO S.Socket
openSocket =
  S.socket S.AF_UNIX S.Stream S.defaultProtocol

withServerSocket :: (S.Socket -> IO c) -> IO c
withServerSocket action =
  bracket openSocket S.close $ \serverSocket ->
    do
      S.bind   serverSocket serverAddress
      S.listen serverSocket S.maxListenQueue
      action   serverSocket

receiveReports :: TQueue EventReport -> IO c
receiveReports reportQueue =
  withServerSocket $ \serverSocket ->
    forever $
    mask $ \unmask ->
      do
        (   clientSocket
          , _clientAddr ) <- S.accept serverSocket
        forkFinally
          ( unmask
            $ receiveReports' clientSocket reportQueue )
          ( \_ -> S.close clientSocket )

receiveReports' :: S.Socket -> TQueue EventReport -> IO ()
receiveReports' clientSocket reportQueue =
  continue
  where
    continue :: IO ()
    continue =
      do
        receivedBytes <- recv clientSocket 1024
        case BS.length receivedBytes of
          0 -> return ()
          _ ->
            do
              receiveReports'' receivedBytes reportQueue
              continue

receiveReports'' :: BS.ByteString
                 -> TQueue EventReport -> IO ()
receiveReports'' receivedBytes reportQueue =
    for_ @[] ( Data.ByteString.Char8.unpack
                receivedBytes ) $ \c ->
        for_ @Maybe ( decodeReport c ) $ \r ->
            atomically $ writeTQueue reportQueue r

---  Analysis of system status changes using
---  event reports  ---

reportWindowSize :: Int
reportWindowSize = 10

okayThreshold :: Ratio Int
okayThreshold = 80 % 100

alarmThreshold :: Ratio Int
alarmThreshold = 50 % 100

analysis :: Seq.Seq EventReport -> Maybe SystemStatus
analysis reports
  | Seq.length reports < reportWindowSize = Nothing
  | successRate <= alarmThreshold         = Just Alarm
  | successRate >= okayThreshold          = Just Okay
  | otherwise                             = Nothing
  where
    successes   = Seq.filter (== Success) reports
    successRate = Seq.length successes % Seq.length reports

analyzeReports :: TQueue EventReport
               -> TQueue SystemStatus -> IO b
analyzeReports reportQueue alarmQueue =
  continue Nothing Seq.empty
  where
    continue :: Maybe SystemStatus
             -> Seq.Seq EventReport -> IO b
    continue status reports =
      do
        newReport <- atomically $ readTQueue reportQueue
        let
          reports' :: Seq.Seq EventReport
          reports' =
            Seq.take reportWindowSize
                     ( newReport Seq.<| reports )

        let
          status' :: Maybe SystemStatus
          status' =
            asum [analysis reports', status]

        for_ @Maybe status' $ \s ->
            when (status /= status')
              $ atomically $ writeTQueue alarmQueue s

        continue status' reports'

---  Sending alerts about system status changes  ---

sendAlarms :: TQueue SystemStatus -> IO b
sendAlarms alarmQueue =
  forever $
    do
      a <- atomically $ readTQueue alarmQueue
      case a of
        Alarm -> putStrLn "Alarm! System is in \
                          \a degraded state."
        Okay  -> putStrLn "System status is normal."

---  Client that sends event reports to ---
---  an aggregation service  ---

sendDemoReportsMain :: IO ()
sendDemoReportsMain = do
  reportQueue <- atomically newTQueue

  foldr1 race_
    [ generateReports reportQueue
    , sendReports reportQueue
    ]

  putStrLn "Done sending demo reports."

---  A fixed schedule of event reports ---
---  for demonstration purposes  ---

demoReports :: [EventReport]
demoReports =
  mapMaybe decodeReport
  "1111111111111010011000001000000100011101111110111111"
  -- successes --     -- failures --    -- successes --

generateReports :: TQueue EventReport -> IO ()
generateReports reportQueue =
  for_ demoReports $ \r ->
    do
      atomically $ writeTQueue reportQueue r
      threadDelay 100_000


---  Sending reports to the server  ---

withClientSocket :: (S.Socket -> IO c) -> IO c
withClientSocket action =
  bracket openSocket S.close $ \clientSocket ->
    do
      S.connect clientSocket serverAddress
      action    clientSocket

sendReports :: TQueue EventReport -> IO c
sendReports reportQueue =
  withClientSocket $ \clientSocket ->
    forever $
      do
        r <- atomically (readTQueue reportQueue)
        sendAll clientSocket
          (Data.ByteString.Char8.pack [encodeReport r])
        putStrLn $ case r of
                     Success -> "1 (success)"
                     Failure -> "0 (failure)"

---  Full demonstration  ---

fullDemonstrationMain :: IO ()
fullDemonstrationMain =
  do
    server <- spawnCommand
      "runhaskell monitoring.hs aggregate-reports"
    threadDelay 1_000_000
    callCommand "runhaskell monitoring.hs send-demo-reports"
    terminateProcess server
    waitForProcess   server
    putStrLn "The full demonstration is complete."
