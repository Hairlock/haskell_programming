import Data.Monoid (Sum, Monoid)
import Data.Semigroup
import MonoidLaws
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial


instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial
    
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main =
    quickCheck (semigroupAssoc :: TrivialAssoc)


-- Identity

newtype Identity a = Identity a

instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- Two
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x y) <> (Two w z) = Two (x <> w) (y <> z)


instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

type TwoAssoc = Two String [Int] -> Two String [Int] -> Two String [Int] -> Bool

-- BoolConj

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    _ <> _ = BoolConj False


instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)


instance Arbitrary BoolConj where
    arbitrary = do
        a <- arbitrary
        elements [(BoolConj a), (BoolConj a)]

-- BoolDisj

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj False) <> (BoolDisj False) = BoolDisj False
    _ <> _ = BoolDisj True

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Arbitrary BoolDisj where
    arbitrary = do
      a <- arbitrary
      elements [(BoolDisj a), (BoolDisj a)]


data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Snd a) <> _ = (Snd a)
    (Fst a) <> (Snd b) = (Snd b)
    (Fst a) <> (Fst b) = (Fst b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
arbitrary = do
    a <- arbitrary
    b <- arbitrary 
    elements [(Fst a), (Snd b)]