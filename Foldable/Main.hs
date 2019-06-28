import Data.Foldable
import Data.Monoid

newtype Min a = Min {getMin :: Maybe a}

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  m `mappend` Min Nothing = m
  Min Nothing `mappend` n = n
  (Min m@(Just x)) `mappend` (Min n@(Just y))
    | x <= y    = Min m
    | otherwise = Min n

-- for definition of xMaximum
newtype Max a = Max {getMax :: Maybe a}

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  m `mappend` Max Nothing = m
  Max Nothing `mappend` n = n
  (Max m@(Just x)) `mappend` (Max n@(Just y))
    | x >= y    = Max m
    | otherwise = Max n

sum :: (Foldable t, Num a) => t a -> a
sum =
    getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product =
    getProduct . foldMap Product
        
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a as =
    getAny $ foldMap (\e -> Any $ a == e) as

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum as =
    getMin $ foldMap (\e -> Min {getMin = Just e}) as


maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum xs = 
    getMax $ foldMap (\e -> Max {getMax = Just e}) xs


null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

length :: (Foldable t) => t a -> Int
length = foldr (\x acc -> acc + 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

xFold :: (Foldable t, Monoid m) => t m -> m
xFold = foldr (\x acc -> x <> acc) mempty -- foldMap id

xFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
xFoldMap f = foldr (mappend . f) mempty

data Constant a b =
    Constant a 
    deriving Show

instance Foldable (Constant b) where
    foldMap _ _ = mempty


data Two a b =
    Two a b

instance Foldable (Two a) where
    foldMap f (Two a b) = f b

data Three a b c =
    Three a b c

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

data Three' a b = Three' a b b

instance Foldable (Three' a) where
    foldMap f (Three' a b c) = (f b) <> (f c)

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
    foldMap f (Four' a b c d) = (f b) <> (f c) <> (f d)

filterF :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterF f = foldMap (\x -> if f x then mempty else pure x)