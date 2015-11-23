 -- Aleksander Eskilson
 -- EECS 776
 -- Homework 5
 -- November 22, 2015
 -- http://adit.io/posts/2013-05-11-The-Dining-Philosophers-Problem-With-Ron-Swanson.html
 {-# LANGUAGE GADTs, KindSignatures #-}

import Control.Concurrent
import Control.Concurrent.STM
import System.Random

type Chop = TMVar Int

main :: IO ()
main = do
    chop0 <- newTMVarIO 0
    chop1 <- newTMVarIO 1
    chop2 <- newTMVarIO 2
    chop3 <- newTMVarIO 3
    chop4 <- newTMVarIO 4

    forkIO (rons 0 (chop0, chop4))
    forkIO (rons 1 (chop1, chop0))
    forkIO (rons 2 (chop2, chop1))
    forkIO (rons 3 (chop3, chop2))
    rons 4 (chop4, chop3)

root :: Int -> (Chop, Chop) -> IO (Int, Int)
root ronId (leftChop, rightChop) = do 
    putStrLn ("Ron #" ++ show ronId ++ " will attempt to eat emulsified beef")
    atomically $ do
        left <- takeTMVar leftChop
        right <- takeTMVar rightChop
        return (left, right)

grub :: Int -> IO ()
grub ronId = do
    p <- randomRIO(1, 10)
    threadDelay (p * 100000)
    putStrLn ("Ron #" ++ show ronId ++ " has eaten Arby's, he will proceed to regret it")

purge :: Int -> (Chop, Chop) -> (Int, Int) -> IO ()
purge ronId (leftChop, rightChop) (left, right) = do
    atomically $ do
        putTMVar leftChop left
        putTMVar rightChop right
    putStrLn ("Ron #" ++ show ronId ++ " has given up on sustaining this inefficient engine\n")
 
rons :: Int -> (Chop, Chop) -> IO ()
rons ronId (leftChop, rightChop) = do 
    -- Grab both chopsticks if possible
    (left, right) <- root ronId (leftChop, rightChop)

    -- Consume like a meat puppet
    grub ronId 

    -- Release chopsticks to your comrades
    purge ronId (leftChop, rightChop) (left, right)

    -- Repeat the cycle of violence
    rons ronId (leftChop, rightChop)

{- When using TMVars, it is possible to group accesses of MVars into single 
 - transactions. TMVars will retry a transaction if the TMVar is empty on a 
 - take, and will retry if the TMVar is full on a put. Each chopstick is
 - given its own TMVar, and accessing both chopsticks is grouped as an
 - atomic transaction. This way, philosophers only get one chopstick if they
 - can get both, avoiding all deadlocks.  MVars can only be accessed in 
 - singular blocking read/writes. Each MVar could be used to store a chopstick,
 - but if access is not grouped atomically, all our Rons might hold a single
 - chopstick and be in deadlock. An MVar could be used as a global spinlock.
 - This would eliminate deadlocks, but would be wasteful of resources, since only
 - one process can be running with both its chopsticks, even if a second process
 - has two chopsticks.
 -}
