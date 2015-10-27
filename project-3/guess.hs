-- Aleksander Eskilson
-- EECS 776
-- Homework 3
-- October 28, 2015
module Main where

import System.Random

main :: IO()
main = do
    gen <- randomRIO (1 :: Int, 10)
    putStrLn "Ha! I've thought of a number between 1 and 10. Bet you can't guess it.."
    guess0 <- readLn
    let loop = (\n g -> if g == n
                        then do
                            putStrLn ("Curses! " ++ show n ++ " is the number!")
                        else do
                            if g < n
                            then do
                                putStrLn "Ha! Your number is puny. Guess a larger number."
                                guess <- readLn
                                loop gen guess
                            else do
                                putStrLn "You choose numbers with meat hands. Pick a smaller number, foolish human."
                                guess <- readLn
                                loop gen guess)
    loop gen guess0

-- All our functions, randomRIO, putStrLn, readLn, return either IO () or IO a.
-- We compose these functions using do notation, which performs their side effects
-- as the functions execute. The loop recursive function takes two integers and also
-- returns IO (). It recurses until the number is guess, printing the ending message,
-- and returning IO () as the final return type of loop, which is also the last return
-- of the main function. 
