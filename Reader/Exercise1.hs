import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled = do
    capped <- cap
    reversed <- rev
    return (capped, reversed)

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> cap <*> rev

tupledM :: [Char] -> ([Char], [Char])
tupledM = cap >>= \capped -> rev >>= \reversed -> return (capped, reversed)