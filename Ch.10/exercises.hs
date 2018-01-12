module Exercises where

    stops = "pbtdkg"
    vowels = "aeiou"


    threeTuple :: [(Char, Char, Char)]
    threeTuple = [(x,y,z) | x <- stops, y <- vowels, z <- stops, x == 'p']

    
    nouns = ["he", "dog", "restaurant", "cat"]
    verbs = ["run", "call", "type"]

    nounVerbNoun = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]


    seekritFunc :: String -> Int
    seekritFunc x =
        div (sum (map length (words x)))
            (length (words x))


    avgWordLength :: Fractional a => String -> a
    avgWordLength sentence = fromIntegral totalLength / fromIntegral numWords
        where 
            wrds = words sentence
            numWords = length wrds
            totalLength = sum $ map length wrds
        

    myOr :: [Bool] -> Bool
    myOr = foldr (||) False


    myAny :: (a -> Bool) -> [a] -> Bool
    myAny f = foldr ((||) . f) False

    myElem :: Eq a => a -> [a] -> Bool
    myElem x = foldr (\i acc -> acc || x == i) False

    myReverse :: [a] -> [a]
    myReverse = foldl (flip (:)) []

    
    myMap :: (a -> b) -> [a] -> [b]
    myMap f = foldr (\x acc -> f x : acc) []

    myFilter :: (a -> Bool) -> [a] -> [a]
    myFilter f = foldr (\x acc -> case f x of
                                    True -> x : acc
                                    _ -> acc) []

    squish :: [[a]] -> [a]
    squish = foldr (++) []
    
    squishMap :: (a -> [b]) -> [a] -> [b]
    squishMap f = foldr ((++) . f) []

    squishAgain :: [[a]] -> [a]
    squishAgain = squishMap id

    myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
    myMaximumBy f xs = foldr (\x acc -> case f x acc of
                                        GT -> x
                                        _ -> acc) (last xs) xs