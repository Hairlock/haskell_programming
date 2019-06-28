module Playground where

import qualified Data.List as L
import Data.Char


main = do
    line <- fmap (L.intersperce . reverse . map toUpper) getLine
    putStrLn line