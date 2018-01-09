module EnumFrom where

    eftBool :: Bool -> Bool -> [Bool]
    eftBool _ False = []
    eftBool True True = [True]
    eftBool False False = [False]
    eftBool b1 b2 = [b1,b2]

    eftOrd :: Ordering -> Ordering -> [Ordering]
    eftOrd LT LT = [LT]
    eftOrd LT EQ = [LT, EQ]
    eftOrd LT GT = [LT, EQ, GT]
    eftOrd EQ LT = []
    eftOrd EQ EQ = [EQ]
    eftOrd EQ GT = [EQ, GT]
    eftOrd GT LT = []
    eftOrd GT EQ = []
    eftOrd GT GT = [GT]

    eftInt :: Int -> Int -> [Int]
    eftInt a b
        | a > b     = []
        | a == b    = [a]
        | otherwise = a : eftInt (succ a) b

    eftChar :: Char -> Char -> [Char]
    eftChar a b
        | a > b     = []
        | a == b    = [a]
        | otherwise = a : eftChar (succ a) b