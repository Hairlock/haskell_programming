import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq a, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type S = String
type B = Bool

quickCheck (monoidAssoc :: S -> S -> S -> B)