module Exercises where

    import Data.Char

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