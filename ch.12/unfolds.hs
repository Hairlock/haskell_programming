module Unfolds where

    myIterate :: (a -> a) -> a -> [a]
    myIterate aToA a = a : myIterate aToA (aToA a)

    as' :: Maybe (a, b) -> [a]
    as' (Just (x, _)) = [x]
    as' Nothing       = []

    bs' :: Maybe (a, b) -> [b]
    bs' (Just (_, y)) = [y]
    bs' Nothing       = []

    myUnfoldr :: (b -> Maybe(a, b)) -> b -> [a]
    myUnfoldr f x = (as' $ f x) ++ myUnfoldr f (head $ bs' $ f x)