module Ch18.Monads where

import Control.Monad

-- bind :: Monad m => (a -> m b) -> m a -> m b
-- bind f = join . fmap f

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else []

data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)
    
instance Functor (Sum a) where
    fmap f (Sum a b) = Sum a (f b)
    
instance Applicative (Sum a) where
    pure = Second
    First x <*> _ = First x
    _ <*> First x = First x
    Second f <*> Second x = Second (f x)

instance Monad (Sum a) where
    return = pure
    -- (>>=) = s