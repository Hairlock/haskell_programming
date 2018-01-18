module Natural where

    data Nat =
        Zero
        | Succ Nat
        deriving (Eq, Show)

    
    natToInteger :: Nat -> Integer
    natToInteger Zero = 0
    natToInteger (Succ n) = natToInteger n + 1

    integerToNat' :: Integer -> Nat
    integerToNat' 0 = Zero
    integerToNat' x = Succ $ integerToNat' (x - 1)


    integerToNat :: Integer -> Maybe Nat
    integerToNat x = case x >= 0 of
        True -> Just (integerToNat' x)
        False -> Nothing