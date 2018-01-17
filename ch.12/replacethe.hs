module ReplaceThe where

    import Data.List

    notThe :: String -> Maybe String
    notThe "the" = Nothing
    notThe s = Just (s ++ " ")

    dropRight :: Int -> [a] -> [a]
    dropRight x = reverse . drop x . reverse

    replaceThe :: String -> String
    replaceThe s = dropRight 1 $ foldr extractJustWords "" notTheWords
        where notTheWords = map notThe $ words s
              extractJustWords = (\s acc -> case s of
                                    Just word -> word ++ acc
                                    Nothing -> "a " ++ acc)