-- A functor is a way to apply a funciton over or around some structure
-- we don't want to alter.

-- Be Kind

-- a -> a, a is *
-- a -> b a -> T (b a), b is * -> *, T is * -> *

-- Function laws
-- fmap id == id
-- fmap (f . g) == fmap f . fmap g

-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- fmap :: Functor f => (m -> n) -> f m -> f n
-- fmap :: Functor f => (x -> y) -> f x -> f y

a = fmap (+1) $ read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = fmap (*2) (\x -> x - 2)

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi     = readIO "1" :: IO Integer
        changed = fmap ("123"++) $ fmap show ioi
    in fmap (*3) $ fmap (\x -> read x :: Integer) changed