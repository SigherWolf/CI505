{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{- HLint is a tool for suggesting possible improvements to Haskell code. 
   These suggestions include ideas such as using alternative functions, 
   simplifying code and spotting redundancies. -}
{-# HLINT ignore "Use lambda-case" #-}
module Week2 where

-- Task 1:
pack :: Eq a => [a] -> [[a]]

-- Case for an empty list
pack [] = []

-- Case for a populated list
pack xs = takeWhile (== head xs) xs : pack (dropWhile (== head xs) xs)

-- Task 2:
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\l -> (length l, head l)) (pack xs)

-- Task 3:
decode :: Eq a => [(Int, a)] -> [a]
decode = concatMap (uncurry replicate)

-- Task 4:
data RLE a = Multiple Int a | Single a deriving Show
encodeRLE :: Eq a => [a] -> [RLE a]
encodeRLE xs = map 
    (\(i, c) -> 
        if i == 1 then Single c 
        else Multiple i c) (encode xs)          

-- Task 5:
decodeRLE :: Eq a => [RLE a] -> [a]
decodeRLE = concatMap 
    (\rle -> case rle of
        Single x -> [x]
        Multiple i x -> replicate i x)