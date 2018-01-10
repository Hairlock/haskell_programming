module Exercises where
    import Data.Char

    uppersInString :: [Char] -> [Char]
    uppersInString s = filter isUpper s

    capFirst :: [Char] -> [Char]
    capFirst (c:cs) = toUpper c : cs


    capRecursive :: [Char] -> [Char]
    capRecursive [] = []
    capRecursive (c:cs) = toUpper c : capRecursive cs

    firstLetter :: [Char] -> Char
    firstLetter = toUpper . head

    myOr :: [Bool] -> Bool
    myOr [] = False
    myOr (b:bs) = b || myOr bs

    myAny :: (a -> Bool) -> [a] -> Bool
    myAny _ [] = False
    myAny f (x:xs) = f x || myAny f xs

    myElem :: Eq a => a -> [a] -> Bool
    myElem _ [] = False
    myElem a (b:bs) = a == b || myElem a bs

    myElemAny :: Eq a => a -> [a] -> Bool
    myElemAny a as = myAny (\x -> a == x) as

    myReverse :: [a] -> [a]
    myReverse [] = []
    myReverse (x:xs) = myReverse xs ++ [x]

    mySquish :: [[a]] -> [a]
    mySquish [] = []
    mySquish (x:xs) = x ++ mySquish xs

    squishMap :: (a -> [b]) -> [a] -> [b]
    squishMap _ [] = []
    squishMap aToBs (a:as) = aToBs a ++ squishMap aToBs as

    squishAgain :: [[a]] -> [a]
    squishAgain as = squishMap (\a -> a) as

    myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
    myMaximumBy _ (m:[]) = m
    myMaximumBy f (x:xs) =
        let
            y = myMaximumBy f xs
        in
            case f x y of
                LT -> y
                EQ -> x
                GT -> x

    myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
    myMinimumBy _ (m:[]) = m
    myMinimumBy f (x:xs) =
        let
            y = myMinimumBy f xs
        in
            case f x y of
                LT -> x
                EQ -> x
                GT -> y

    myMaximum :: (Ord a) => [a] -> a
    myMaximum = myMaximumBy compare
    
    myMinimum :: (Ord a) => [a] -> a
    myMinimum = myMinimumBy compare                