module Exercises where

    mc91 :: Integer -> Integer
    mc91 a
      | a > 100 = a - 10
      | otherwise = mc91 . mc91 $ a + 11