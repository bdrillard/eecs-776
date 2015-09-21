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

-- Prelude> :load rot13.hs
-- Ok, modules loaded: Main.
-- Prelude Main> rot13 "A"
-- "N"
-- Prelude Main> rot13 "N"
-- "A"
-- Prelude Main> rot13 "Z"
-- "M"
-- Prelude Main> rot13 "M"
-- "Z"
-- Prelude Main> rot13 "STRING"
-- "FGEVAT"
-- Prelude Main> rot13 "FGEVAT"
-- "STRING"
-- Prelude Main> rot13 "TWICE NIGHTLY SCREENING OF MY FILMS IN TRENCHES EXCELLENT IDEA STOP BUT MUST INSIST THAT E BLACKADDER BE PROJECTIONIST STOP P S DONT LET HIM EVER STOP"
-- "GJVPR AVTUGYL FPERRAVAT BS ZL SVYZF VA GERAPURF RKPRYYRAG VQRN FGBC OHG ZHFG VAFVFG GUNG R OYNPXNQQRE OR CEBWRPGVBAVFG FGBC C F QBAG YRG UVZ RIRE FGBC"
-- Prelude Main> rot13 "GJVPR AVTUGYL FPERRAVAT BS ZL SVYZF VA GERAPURF RKPRYYRAG VQRN FGBC OHG ZHFG VAFVFG GUNG R OYNPXNQQRE OR CEBWRPGVBAVFG FGBC C F QBAG YRG UVZ RIRE FGBC"
-- "TWICE NIGHTLY SCREENING OF MY FILMS IN TRENCHES EXCELLENT IDEA STOP BUT MUST INSIST THAT E BLACKADDER BE PROJECTIONIST STOP P S DONT LET HIM EVER STOP"
