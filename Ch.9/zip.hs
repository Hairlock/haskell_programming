module Zip where

    myZip :: [a] -> [b] -> [(a, b)]
    myZip [] _ = []
    myZip _ [] = []
    myZip (a:as) (b:bs) = [(a, b)] : myZip as bs 