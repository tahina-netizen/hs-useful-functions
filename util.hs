module Util where

import Data.Maybe
import Data.List(unfoldr)

{- 
Gives the list of all digits of n from the leftest digit to the rightest 
digit.
E.g:
intToDec 0 = [0]
intToDec 42 = [4, 2]
intToDec (-423) = [4, 2, 3]
-}
intToDec :: Integral a => a -> [a]
intToDec 0 = [0]
intToDec n = unfoldr f (abs n)
    where
        f :: Integral a =>  a -> Maybe (a, a)
        f 0 = Nothing
        f m = let (left, len) = leftestDigit m 
                in Just (left, m - left * (10 ^ (len - 1)))

{-
Gives (a, b) where a is the leftest digit of z and b the length of z
(the length of a digit is 1).
E.g:
leftestDigit 0 = (0, 1)          
leftestDigit 423 = (4, 3)
-}
leftestDigit :: Integral a => a -> (a, a) 
leftestDigit z 
    | z `elem` [0..9] = (z, 1)
    | otherwise = (a, b + 1)
        where (a, b) = leftestDigit (z `div` 10)