module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n =
    let wordArray = ["zero", "one", "two", "three", "four", "five",
                     "six", "seven", "eight", "nine", "ten"]
    in
         wordArray!!n


digits :: Int -> [Int] 
digits n
    | n == 0 = []
    | otherwise = digits (div n 10) ++ [mod n 10]


wordNumber :: Int -> String 
wordNumber = concat . intersperse "-" . map digitToWord . digits