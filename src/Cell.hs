module Cell (
    CellCord (CellCord),
    FuncParam (RangeParam, ListParam),
    CellContent (StringCont, NumberCont, SUMFunc, MULFunc, AVGFunc),
    Cell (Cell)
) where

-- położenie (0,0) to lewy górny róg
-- x - kolumna
-- y - wiersze
data CellCord = CellCord Int Int deriving (Eq, Show)

-- argument funckji
-- RangeParam - zakres
-- ListParam - lista komórek
data FuncParam = RangeParam CellCord CellCord | ListParam [CellCord] deriving Show

-- zawartość (coś ala typ + dane)
-- StringCont - napis
-- NumberCont - liczba
-- SUMFunc - funckja sumy
data CellContent = StringCont String | NumberCont Int | SUMFunc FuncParam | MULFunc FuncParam | AVGFunc FuncParam deriving Show

-- pełna komórka
-- CellCord - położenie
-- CellContent - zawartość przetworzona
-- String - napis oryginalny
data Cell = Cell CellCord CellContent String deriving Show
