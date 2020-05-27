{-# LANGUAGE NumericUnderscores #-}

import Control.Concurrent       ( threadDelay )
import Control.Concurrent.Async ( forConcurrently_ )
import Control.Concurrent.STM   ( atomically, newTBQueue
                                , readTBQueue, writeTBQueue )
import Control.Exception.Safe   ( bracket_ )
import System.IO                ( BufferMode(LineBuffering)
                                , hSetBuffering, stdout )

import qualified System.Random.MWC as R ( GenIO, uniformR
                                        , createSystemRandom )

vendorApiCall :: R.GenIO -> Int -> IO Int
vendorApiCall rng n =
  do
    t <- R.uniformR (500_000, 1_500_000) rng
    threadDelay t
    return ( n * n )

main :: IO ()
main =
  do
    hSetBuffering stdout LineBuffering

    rng <- R.createSystemRandom

    bq  <- atomically $ newTBQueue 5
    let
      acquire :: IO ()
      acquire = atomically $ writeTBQueue bq ()
      release :: IO ()
      release = atomically $ readTBQueue  bq

    forConcurrently_ [1 .. 10] $ \x ->
      bracket_ acquire release $
        do
          putStrLn $ "start: "
                  ++ show x
          result <- vendorApiCall rng x
          putStrLn $ "finish: "
                  ++ show x
                  ++ ", result: "
                  ++ show result
