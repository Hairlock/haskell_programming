module Cipher where

import Data.Char

ordDiff :: Char -> Int
ordDiff ch = ord ch - ord 'a'

ordToChar :: Int -> Char
ordToChar n = chr (ord 'a' + n)

shift :: (Int -> Int -> Int) -> Int -> Char -> Char
shift f n ch = ordToChar $ mod (ordDiff ch `f` n) 26

rShift = shift (+)

lShift = shift (-)

caesar :: Int -> [Char] -> [Char]
caesar _ [] = []
caesar n (' ':cs) = ' ' : caesar n cs
caesar n (c:cs) = rShift n c : caesar n cs

unCaesar :: Int -> [Char] -> [Char]
unCaesar _ [] = []
unCaesar n (' ':cs) = ' ' : unCaesar n cs
unCaesar n (c:cs) = lShift n c : unCaesar n cs
    
