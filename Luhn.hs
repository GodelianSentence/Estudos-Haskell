-- Esta é uma implementação do algoritmo de Luhn em Haskell

module Luhn where

digitosRev     :: Int -> [Int]
dobroAlternado :: [Int] -> [Int]
somaDigitos    :: [Int] -> Int
luhn           :: Int -> Bool

digitosRev n
   | n < 10    = [n]
   | otherwise = (n `mod` 10) : digitosRev(n `div` 10)

dobroAlternado []          = []
dobroAlternado [x]         = [x]
dobroAlternado [x,y]       = [x, y * 2]
dobroAlternado (x : y : z) = x : 2 * y : dobroAlternado z

somaDigitos [x]      = sum (digitosRev x)
somaDigitos (x : xs) = sum (digitosRev x) + somaDigitos xs

luhn n
   | somaDigitos (dobroAlternado (digitosRev n)) `mod` 10 == 0 = True
   | otherwise                                                 = False
