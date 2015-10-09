-- Aleksander Eskilson
-- EECS 776
-- Homework 2
-- October 9, 2015
{-# LANGUAGE GADTs, KindSignatures #-}

module Main where

data Topping :: * where
    Pepperoni :: Topping
    Sausage   :: Topping
    Beef      :: Topping
    Olives    :: Topping
    Pineapple :: Topping
    Ham       :: Topping
    Peppers   :: Topping
    deriving (Show)

data Sauce :: * where
    Marinara :: Sauce
    Alfredo  :: Sauce
    deriving (Show)

data Size = Small | Medium | Large
    deriving (Show)

data Crust :: * where
    Thin    :: Crust
    Regular :: Crust
    Stuffed :: Crust
    deriving (Show)

data Pizza :: * where
    Pizza :: Size -> Crust -> Sauce -> [Topping] -> Pizza

showToppings :: [Topping] -> String
showToppings (t:[]) = "and " ++ show t ++ " as toppings!"
showToppings (t:ts) = show t ++ ", " ++ showToppings ts

instance Show Pizza where
    show (Pizza sz c sc ts) = "Wow! You got a " ++ show sz ++ " pizza on " ++ 
                              show c ++ " crust with " ++ show sc ++ " sauce and " ++ 
                              showToppings ts

main :: IO()
main = do
    let pizza1 = Pizza Small Thin Marinara [Beef, Sausage, Olives]
        pizza2 = Pizza Medium Regular Marinara [Pepperoni, Pineapple]
        pizza3 = Pizza Large Stuffed Alfredo [Ham, Peppers, Sausage]
    putStrLn (show pizza1)
    putStrLn (show pizza2)
    putStrLn (show pizza3)

-- Wow! You got a Small pizza on Thin crust with Marinara sauce and Beef, Sausage, and Olives as toppings!
-- Wow! You got a Medium pizza on Regular crust with Marinara sauce and Pepperoni, and Pineapple as toppings!
-- Wow! You got a Large pizza on Stuffed crust with Alfredo sauce and Ham, Peppers, and Sausage as toppings!
