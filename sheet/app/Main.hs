module Main where

import           Cell
import           CellParser
import           IOUtil
import           System.Console.ANSI

main :: IO ()
main = do
    clearScreen
    setCursorPosition 1 0
    setTitle "ANSI Terminal Short Example"

    setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid Red
           ]
    putStr $ show $ parseCell "SUM(A2:A3)"
    putStr $ show $  Cell (FuncCell SUMFunc [RangeParam (CellCord 1 2) (CellCord 1 3)] Nothing) "SUM(A2:A3)"
    setSGR [Reset]


