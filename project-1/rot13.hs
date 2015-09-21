module Main where

import Data.Char

main :: IO()
main = do
    putStrLn "Enter a string using capital English letters and spaces to encode:"
    str <- getLine
    putStrLn $ rot13 str

conv :: Char -> Char
conv ' ' = ' '
conv c = chr (((ord c - 65 + 13) `mod` 26) + 65)

rot13 :: [Char] -> [Char]
rot13 cs = map conv cs
