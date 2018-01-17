module TheVowel where

    countTheBeforeVowels' :: [String] -> Integer -> Integer
    countTheBeforeVowels' [] c = c
    countTheBeforeVowels' ("the":ws) c = countTheBeforeVowels' ws count
        where count = if (head . head $ ws) `elem` ['a','e','i','o','u'] then c + 1 else c
    countTheBeforeVowels' (_:ws) c = countTheBeforeVowels' ws c

        
    countTheBeforeVowels :: String -> Integer
    countTheBeforeVowels s = countTheBeforeVowels' (words s) 0