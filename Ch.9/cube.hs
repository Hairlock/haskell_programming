module Cube where

    mySqr = [x^2 | x <- [1..5] ]
    myCube = [x^3 | x <- [1..5] ]

    tuples = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]