module Cell (
    CellCord (CellCord),
    FuncParam (RangeParam, OneCell),
    NumberType (IntVal, DecimalVal),
    FuncName (SUMFunc, MULFunc, AVGFunc),
    ErrorType (ParseFail, CyclicDependency),
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

-- rodzaj liczby
-- IntVal - całkowita
-- DecimalVal - stało przecinkowa
data NumberType = IntVal Int | DecimalVal Rational deriving (Eq, Show)

-- funkcja
data FuncName = SUMFunc | MULFunc | AVGFunc deriving (Eq, Show)

-- rodzaj błędu
data ErrorType = ParseFail | CyclicDependency  deriving (Eq, Show)


-- zawartość (coś ala typ + dane)
-- FuncCell - funkcja
-- StringCont - napis
-- NumberCont - liczba
-- ErrorCell - błąd
data CellContent = FuncCell FuncName [FuncParam] | NumberCell NumberType | StringCell String | ErrorCell ErrorType String deriving (Eq, Show)

-- pełna komórka
-- CellCord - położenie
-- CellContent - zawartość przetworzona
-- String - napis oryginalny
data Cell = Cell CellCord CellContent String deriving (Eq, Show)
