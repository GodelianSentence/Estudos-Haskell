-- Implementação do algoritmo Merge Sort

module Msort where

merge :: Ord a => [a] -> [a] -> [a]
halve :: [a] -> ([a],[a])
msort :: Ord a => [a] -> [a]

merge [] ys    = ys
merge xs []    = xs
merge (x:xs) (y:ys)
   | x <= y    = x : merge xs (y:ys)
   | otherwise = y : merge (x:xs) ys

halve l = (take n l,drop n l)
   where
      n = length l `div` 2

msort []           = []
msort [x]          = [x]
msort (x:xs)       = merge (msort left) (msort right)
   where
     (left, right) = halve (x:xs)
