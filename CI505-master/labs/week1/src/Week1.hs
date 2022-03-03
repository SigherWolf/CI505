module Week1 where

import Prelude hiding (elem, take, length, drop)

-- First ever Haskell code that I wrote :)
test :: String
test = "Hello World"

-- Task 1:
square :: Int -> Int
square x = x * x

-- Task 2:
sumsquare :: Int -> Int -> Int
sumsquare x y = square x + square y

-- Task 3:
elem :: Eq a => a -> [a] -> Bool
elem x [] = False
elem x (y:ys) = True

-- Task 4:
length :: [a] -> Int
length [] = 0
length (x:xs) = length xs + 1

-- Task 5:
drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop n [] = []
drop n (x:xs) = drop (n - 1) xs

-- Task 6:
take :: Int -> [a] -> [a]
take 0 xs = xs
take n [] = []
take n (x:xs) = x : take (n - 1) xs
