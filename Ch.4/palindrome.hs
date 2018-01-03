
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x


myAbs :: Integer -> Integer
myAbs i = 
    if i < 0 then
        i * (-1)
    else
        i

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))

add = (+)
f2 s = w 
    where w = add (length s) 1


firstOfList (x : xs) = x