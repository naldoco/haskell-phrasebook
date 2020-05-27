import Control.Concurrent          ( forkIO )
import Control.Concurrent.STM.TVar ( newTVar, readTVar,
                                     modifyTVar' )
import Control.Monad.STM           ( atomically, check )
import Data.Foldable               ( for_ )
import System.IO        ( BufferMode(LineBuffering),
                                     hSetBuffering, stdout )

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  tasksCompleted <- atomically (newTVar 0)

  let
    task :: String -> IO ()
    task x =
      do
        for_ [1..3] $ \i ->
          putStrLn $ x
                  ++ ": "
                  ++ show i
        atomically
          $ modifyTVar' tasksCompleted (+ 1)

  task "main"
  forkIO $ task "forkA"
  forkIO $ task "forkB"

  atomically $
    do
      x <- readTVar tasksCompleted
      check $ x == 3

  putStrLn "done"
