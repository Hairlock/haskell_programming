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