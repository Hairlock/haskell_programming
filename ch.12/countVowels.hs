module CountVowels where

    countVowels' :: String -> Integer -> Integer
    countVowels' [] count = count
    countVowels' (x:xs) count = countVowels' xs c
        where c = if x `elem` "aeiou" then count + 1 else count


    countVowels :: String -> Integer
    -- countVowels s = countVowels' s 0
    countVowels s = foldr (\x acc -> if x `elem` "aeiou" then acc + 1 else acc) 0 s