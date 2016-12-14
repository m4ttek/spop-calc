module Cell (
    CellCord (CellCord),
    FuncParam (RangeParam, ListParam),
    NumberType (Int, Double),
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
data FuncParam = RangeParam CellCord CellCord | ListParam [CellCord] deriving Show

data NumberType = Int | Double deriving Show
data FuncName = SUMFunc | MULFunc  | AVGFunc deriving (Eq, Show)


-- zawartość (coś ala typ + dane)
-- StringCont - napis
-- IntCont - liczba całkowita
-- DoubleCont - liczba zmiennno przecinkowa
-- SUMFunc - funckja sumy
data CellContent = FuncCell FuncName [FuncParam] | NumberCell NumberType | StringCell String | ErrorCell String deriving Show

-- pełna komórka
-- CellCord - położenie
-- CellContent - zawartość przetworzona
-- String - napis oryginalny
data Cell = Cell CellCord CellContent String deriving Show
