module Exercises where

    printIt a = print a

    atPosition4 = [(!!) "Curry is awesome!" 4]

    lastWord word = drop 9 word

    thirdChar :: String -> Char
    thirdChar s = (!!) s 3


    thirdCharWithindex n = (!!) "Curry is awesome" n