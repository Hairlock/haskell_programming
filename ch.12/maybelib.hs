module MaybeLib where

    isJust :: Maybe a -> Bool
    isJust (Just a) = True
    isJust _ = False

    
    isNothing :: Maybe a -> Bool
    isNothing = not . isJust


    mayybee :: b -> (a -> b) -> Maybe a -> b
    mayybee b aToB (Just mbA) = aToB mbA
    mayybee b aToB Nothing = b


    fromMaybe :: a -> Maybe a -> a
    fromMaybe a Nothing = a
    fromMaybe a (Just v) = v


    listToMaybe :: [a] -> Maybe a
    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x


    maybeToList :: Maybe a -> [a]
    maybeToList Nothing = []
    maybeToList (Just x) = [x]


    catMaybes :: [Maybe a] -> [a]
    catMaybes as = foldr (\x acc -> case x of 
                                        Just a -> a : acc
                                        Nothing -> acc) [] as


    flipMaybe :: [Maybe a] -> Maybe [a]
    flipMaybe as = case catMaybes as of
                    [] -> Nothing
                    a -> Just a