module DetermineTheType where

    -- example = head [(0 :: Integer, "doge"), (1, "kitten")]
    -- example = if False then True else False
    example = (length [1, 2, 3, 4, 5])

    functionH :: [a] -> a
    functionH (x:_) = x

    functionC :: Ord a => a -> a -> Bool
    functionC x y = if (x > y) then True else False

    i :: a -> a
    i a = a

    c :: a -> b -> a
    c a b = a

    c'' :: b -> a -> b
    c'' b a = b

    c' :: a -> b -> b
    c' a b = b

    r :: [a] -> [a]
    r a = a

    co :: (b -> c) -> (a -> b) -> a -> c
    co bToC aToB a =  bToC $ aToB $ a

    a :: (a -> c) -> a -> a
    a aToC a = a

    a' :: (a -> b) -> a -> b
    a' aToB a = aToB a