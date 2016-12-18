module Cell (
    CellCord (CellCord),
    FuncParam (RangeParam, OneCell),
    NumberType (IntVal, DecimalVal),
    FuncName (SUMFunc, MULFunc, AVGFunc),
    CellContent(FuncCell, NumberCell, StringCell, ErrorCell),
    Cell (Cell)
) where

-- położenie (0,0) to lewy górny róg
-- x - kolumna
-- y - wiersze
data CellCord = CellCord Int Int deriving (Eq, Show)

-- argument funckji
-- RangeParam - zakres
-- ListParam - lista komórek
data FuncParam = RangeParam CellCord CellCord | OneCell CellCord deriving (Eq, Show)


data NumberType = IntVal Int | DecimalVal Rational deriving (Eq, Show)
data FuncName = SUMFunc | MULFunc | AVGFunc deriving (Eq, Show)


-- zawartość (coś ala typ + dane)
-- FuncCell
-- StringCont - napis
-- IntCont - liczba całkowita
-- DoubleCont - liczba zmiennno przecinkowa
-- SUMFunc - funckja sumy
data CellContent = FuncCell FuncName [FuncParam] | NumberCell NumberType | StringCell String | ErrorCell String deriving (Eq, Show)

-- pełna komórka
-- CellCord - położenie
-- CellContent - zawartość przetworzona
-- String - napis oryginalny
data Cell = Cell CellCord CellContent String deriving Show
