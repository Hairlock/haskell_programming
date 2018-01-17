module Exercises where

    import Data.Char
    import Data.List

    isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
    isSubsequenceOf [] [] = error "emptry strings not allowed"
    isSubsequenceOf xs@(x:restxs) ys@(y:restys)
        | x == y = isSubsequenceOf restxs ys
        | otherwise = isSubsequenceOf xs restys
    isSubsequenceOf [] (y:ys) = True
    isSubsequenceOf (x:xs) [] = False


    capitalizeWords :: String -> [(String, String)]
    capitalizeWords s = foldr (\w@(x:xs) acc ->
                                    (w, toUpper x:[] ++ xs) : acc) [] $ words s


    capitalizeWord :: String -> String
    capitalizeWord (x:xs) = toUpper x : [] ++ xs

    splitOn :: Char -> String -> [String]
    splitOn c [] = []
    splitOn c xs  = (takeWhile (/=c) xs) : splitOn c (drop 1 $ dropWhile (/=c) xs)

    capitalizeParagraph :: String -> String
    capitalizeParagraph x = (concat $ intersperse ". " capitals) ++ "."
        where capitals  = map capitalizeWord separated
              separated = splitOn '.' x

