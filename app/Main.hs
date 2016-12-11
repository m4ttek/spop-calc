module Main where

import           Cell
import           Lib


main :: IO ()
main = putStrLn $ show $  Cell (CellCord 1 2) (SUMFunc (RangeParam (CellCord 1 2) (CellCord 1 3))) "SUM(A2:A3)"
