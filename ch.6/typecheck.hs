module TypeCheck where
    import Data.List

    data Mood = Blah | Woot deriving (Eq, Show, Ord)

    settleDown x = if x == Woot
                      then Blah
                      else x

    
    i :: Num a => a
    i = 1

    f :: Float
    f = 1.0

    jung :: [Int] -> Int
    jung xs = head (sort xs)

    -- young :: [Char] -> Char
    young :: Ord a => [a] -> a
    young xs = head (sort xs)

    mySort :: [Char] -> [Char]
    mySort = sort

    signifier :: [Char] -> Char
    -- signifier :: Ord a => [a] -> a
    signifier xs = head (mySort xs)

    chk :: Eq b => (a -> b) -> a -> b -> Bool
    chk aToB a b = (aToB a) == b

    arith :: Num b => (a -> b) -> Integer -> a -> b
    arith aToB n a = (aToB a) + (fromInteger n)