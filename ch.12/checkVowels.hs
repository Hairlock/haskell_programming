module CheckVowels where

    newtype Word' = 
        Word' String
        deriving (Eq, Show)
    
    vowels = "aeiou"
    
    checkChar :: Char -> (Integer, Integer) -> (Integer, Integer)
    checkChar ch (v,c)
        | ch `elem` vowels = (v + 1, c)
        | ch == ' ' = (v,c)
        | otherwise = (v, c + 1)

    
    countVowelsAndConsonants :: String -> Maybe Word'
    countVowelsAndConsonants "" = error "enter a string"
    countVowelsAndConsonants s = if (fst counts) > (snd counts) then
                                    Just $ Word' s
                                 else
                                    Nothing
        where counts = foldr checkChar (0, 0) s