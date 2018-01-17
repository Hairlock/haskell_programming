module ReplaceThe where

    notThe :: String -> Maybe String
    notThe "the" = Nothing
    notThe s = Just s

    replaceThe :: String -> String
    replaceThe s = foldr (\s acc ->
                            case s of
                                Just word -> word ++ " " ++ acc
                                Nothing -> acc) " " $ map notThe $ words s