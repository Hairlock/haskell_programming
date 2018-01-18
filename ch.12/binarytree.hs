module BinaryTree where

    data BinaryTree a =
        Leaf
        | Node (BinaryTree a) a (BinaryTree a)
        deriving (Eq, Ord, Show)

    
    unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
    unfold f a = 
        case f a of
            Nothing -> Leaf
            Just (l, m, r) -> Node (unfold f l) m (unfold f r)


    treeBuild :: Integer -> BinaryTree Integer
    treeBuild i
        | i < 0 = Leaf
        | otherwise = unfold f 0
            where
                f k
                    | k == i = Nothing
                    | otherwise = Just (k+1, k, k+1)