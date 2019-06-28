module Exercises where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a =
    NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return = pure
    (>>=) _ _ = NopeDotJpg


instance Arbitrary a => Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg    

instance Eq a => EqProp (Nope a) where
    (=-=) = eq    

data PhhhbtEither a b =
    Left' a
    | Right' b deriving (Eq, Show)
    
instance Functor (PhhhbtEither a) where
    fmap f (Left' a) = Left' a
    fmap f (Right' b') = Right' (f b')