module DayOfTheWeek where

    data DayOfWeek =
        Mon | Tue | Weds | Thu | Fri | Sat | Sun 
        deriving (Show)

    -- day of week and numerical day of month
    data Date =
        Date DayOfWeek Int deriving (Show)

    instance Eq DayOfWeek where
        (==) Mon Mon = True
        (==) Tue Tue = True
        (==) Weds Weds = True
        (==) Thu Thu = True
        (==) Fri Fri = True
        (==) Sat Sat = True
        (==) Sun Sun = True
        (==) _ _ = False

    instance Ord DayOfWeek where
        compare Fri Fri = EQ
        compare Fri _ = GT
        compare _ Fri = LT
        compare _ _ = EQ        

    instance Eq Date where
        (==) (Date weekday dayOfMonth)
             (Date weekday' dayOfMonth') =
            weekday == weekday' && dayOfMonth == dayOfMonth'
               
    data Identity a =
        Identity a

    instance Eq a => Eq (Identity a) where
        (==) (Identity v) (Identity v') = v == v'


    check' :: Ord a => a -> a -> Bool
    check' a a' = a == a'