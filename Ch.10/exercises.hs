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
    myElem x = foldr (\i acc -> x == i) False