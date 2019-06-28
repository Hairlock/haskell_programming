import Control.Applicative
import Data.List (elemIndex)

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z



a :: Maybe Int
a = elemIndex 3 [1..5]

b :: Maybe Int
b = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> a <*> b

xs = [1, 2, 3]
ys = [4, 5, 6]

c :: Maybe Integer
c = lookup 3 $ zip xs ys

d :: Maybe Integer
d = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> c <*> d

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)


newtype Constant a b =
    Constant { getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap f (Constant b) = Constant b
    
instance Monoid a => Applicative (Constant a) where
    pure _ = Constant { getConstant = mempty }
    (<*>) (Constant x) (Constant y) = Constant (mappend x y)


const <*> Just "Hello" <$> "World"