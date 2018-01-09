module While where

    myWords :: String -> [String]
    myWords [] = []
    myWords (' ':s) = myWords s
    myWords s = start : myWords end
        where
            start = takeWhile (/=' ') s
            end = dropWhile (/=' ') s
