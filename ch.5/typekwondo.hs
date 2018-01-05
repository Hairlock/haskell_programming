module TypeKwonDo where

    h :: Int -> Char
    h i = g $ f $ i

    e :: A -> C
    e a = w $ q $ a

    xform :: (X, Y) -> (Z, Z)
    xform (x, y) = (xz x, yz y)

    munge :: (x -> y) -> (y -> (w, z)) -> x -> w
    munge xToY yToWZ x = fst $ yToWZ $ xToY $ x