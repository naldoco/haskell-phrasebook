import Control.Monad.STM           ( atomically, check )
import Control.Concurrent.STM.TVar ( newTVar, readTVar
                                   , modifyTVar' )

import Control.Concurrent          ( forkIO, threadDelay )

import System.Random.MWC           ( Gen, createSystemRandom
                                   , uniformR )

import qualified Data.Sequence as Seq ( Seq, fromList
                                      , index, length )

import Control.Monad               ( forever )
import Data.Foldable               ( asum, for_ )
import Data.Traversable            ( for )
import GHC.Conc.Sync               ( TVar )
import Control.Monad.Primitive     ( PrimMonad, PrimState )

main :: IO ()
main = do
  accountList <-
    for [1..10] $ \_ ->
      atomically $ newTVar (100 :: Integer)

  let
    accountSeq :: Seq.Seq (TVar Integer)
    accountSeq =
      Seq.fromList accountList

    randomAccount :: (PrimMonad m)
                  => Gen (PrimState m) -> m (TVar Integer)
    randomAccount rng =
      do
        i <- uniformR (1, Seq.length accountSeq) rng
        return (Seq.index accountSeq (i - 1))

  for_ [1..500] $ \_ ->
    forkIO $
      do
        rng <- createSystemRandom
        forever $
          do
            d <- uniformR (10, 50) rng
            threadDelay d

            sender    <- randomAccount rng
            recipient <- randomAccount rng

            amount <-
              do
                x <- uniformR (1, 10) rng
                return $ toInteger (x :: Int)

            atomically $
              asum
                [ do
                    modifyTVar' sender (\x -> x - amount)
                    readTVar    sender >>=
                                        \x -> check (x >= 0)

                    modifyTVar' recipient (\x -> x + amount)

                , return () ]

  for_ [1..4] $ \_ ->
    do
      threadDelay 500000
      balances <- atomically $ for accountList readTVar
      putStrLn $ show balances
      putStrLn $ "Total: "
              ++ show ( sum balances )
