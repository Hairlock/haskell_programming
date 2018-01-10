module Zip where

    myZip :: [a] -> [b] -> [(a, b)]
    myZip [] _ = []
    myZip _ [] = []
    myZip (a:as) (b:bs) = (a, b) : myZip as bs

    myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    myZipWith _ [] _ = []
    myZipWith _ _ [] = []
    myZipWith f (a:as) (b:bs) = a `f` b : myZipWith f as bs