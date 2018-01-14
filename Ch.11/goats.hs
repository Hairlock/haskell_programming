{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Goats where

    class TooMany a where
        tooMany :: a -> Bool

    instance TooMany Int where
        tooMany n = n > 42

    instance TooMany (Int, String) where
        tooMany 


    newtype Goats = 
        Goats Int deriving (Eq, Show, TooMany)


    -- instance TooMany Goats where
    --     tooMany (Goats n) = n > 43
