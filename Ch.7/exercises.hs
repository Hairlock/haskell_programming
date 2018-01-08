module Exercises where

    f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
    f (a, b, c) (d, e, f) = ((a, d), (c, f))

    functionC x y = case x > y of
                        True -> x
                        _ -> y

    ifEvenAdd2 n = case even n of
                        True -> n + 2
                        _ -> n

    dodgy :: Num a => a -> a -> a
    dodgy x y = x + y * 10

    oneIsOne :: Num a => a -> a
    oneIsOne = dodgy 1

    oneIsTwo = (flip dodgy) 2


    tensDigit :: Integral a => a -> a
    tensDigit x = d
        where d = fst $ divMod x 10

    
    foldBool3Case :: a -> a -> Bool -> a
    foldBool3Case x y t = case t of
                            True -> x
                            _ -> y

    
    foldBool3Guard :: a -> a -> Bool -> a
    foldBool3Guard x y t
        | t == True = x
        | otherwise = y

    
    g :: (a -> b) -> (a, c) -> (b, c)
    g aToB (a, c) = (aToB a, c)


    sumToN :: (Eq a, Num a) => a -> a
    sumToN 0 = 0
    sumToN 1 = 1
    sumToN n = n + sumToN (n - 1)


    data DividedResult =
        Result (Integer, Integer)
        | DividedByZero
        deriving (Show)

    sign x y
        | x*y < 0 = -1
        | otherwise = 1
    
    dividedBy :: Integer -> Integer -> DividedResult
    dividedBy _ 0 = DividedByZero
    dividedBy num denom = 
        let
            (a, b) = go (abs num) (abs denom) 0
            go n d count
                | n < d = (count, n) 
                | otherwise = go (n - d) d (count + 1)
        in
            Result ((sign num denom) * a, b)
