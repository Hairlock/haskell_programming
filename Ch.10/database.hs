module Database where
    import Data.Time

    data DatabaseItem = DbString String
                        | DbNumber Integer
                        | DbDate UTCTime
                        deriving (Eq, Ord, Show)

    
    theDatabase :: [DatabaseItem]
    theDatabase =
        [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
        , DbNumber 9001
        , DbString "Hello, world!"
        , DbDate (UTCTime
                    (fromGregorian 1921 5 1)
                    (secondsToDiffTime 34123))
        ]


    filterDb :: (DatabaseItem -> [a]) -> [DatabaseItem] -> [a]
    filterDb _ [] = []
    filterDb f xs = foldr (\x y -> f x ++ y) [] xs


    byTime :: DatabaseItem -> [UTCTime]
    byTime i = case i of
                    DbDate d ->
                        d : []
                    _ ->
                        []

    
    byNumber :: DatabaseItem -> [Integer]
    byNumber i = case i of
                    DbNumber n ->
                        n : []
                    _ ->
                        []

    
    filterDbDate :: [DatabaseItem] -> [UTCTime]
    filterDbDate = filterDb byTime


    filterDbNumber :: [DatabaseItem] -> [Integer]
    filterDbNumber = filterDb byNumber

    
    mostRecent :: [DatabaseItem] -> UTCTime
    mostRecent = maximum . filterDbDate


    sumDb :: [DatabaseItem] -> Integer
    sumDb = sum . filterDbNumber

    avg :: Fractional a => [a] -> a
    avg xs = (sum xs) / fromIntegral (length xs)

    avgDb :: [DatabaseItem] -> Double
    avgDb items = avg $ map fromInteger $ filterDbNumber items