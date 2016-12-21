module Cell (
    CellCord (CellCord),
    FuncParam (RangeParam, OneCell),
    FuncParams,
    doesParamContainsCord,
    NumberType (IntVal, DecimalVal),
    FuncName (SUMFunc, MULFunc, AVGFunc),
    ErrorType (ParseFail, CyclicDependency),
    CellContent(FuncCell, NumberCell, StringCell, ErrorCell),
    Cell (Cell),
    containsCord
) where

-- położenie (0,0) to lewy górny róg
-- x - kolumna
-- y - wiersze
data CellCord = CellCord Int Int deriving (Eq, Show)

getColumn :: CellCord -> Int
getColumn (CellCord col _) = col

getRow :: CellCord -> Int
getRow (CellCord _ row) = row

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

type FuncParams = [FuncParam]

-- zawartość (coś ala typ + dane)
-- FuncCell - funkcja
-- StringCont - napis
-- NumberCont - liczba
-- ErrorCell - błąd
data CellContent = FuncCell FuncName FuncParams | NumberCell NumberType | StringCell String | ErrorCell ErrorType String deriving (Eq, Show)

-- pełna komórka
-- CellCord - położenie
-- CellContent - zawartość przetworzona
-- String - napis oryginalny
data Cell = Cell CellCord CellContent String deriving (Eq, Show)


 -- funkcje


doesParamContainsCord :: CellCord -> FuncParam -> Bool
doesParamContainsCord cord (RangeParam x y) 
-- można to przerobic na highorder function ale będzie w kij nieczytelne (chyba)
    = let insideColumn = if getColumn x > getColumn y then --kolumna x wieksza niz y
                            getColumn cord >= getColumn y && getColumn cord <= getColumn x
                         else -- kolumna y wieksza niz x
                            getColumn cord >= getColumn x && getColumn cord <= getColumn y
          insideRow = if getRow x > getRow y then
                         getRow cord >= getRow y && getRow cord <= getRow x
                      else
                         getRow cord >= getRow x && getRow cord <= getRow y
     in insideColumn && insideRow
doesParamContainsCord cord (OneCell x) = getColumn cord == getColumn x && getRow cord == getRow x


containsCord :: CellCord -> FuncParams -> Bool
containsCord x [] = False
containsCord x params = foldl (||) False (map (doesParamContainsCord x) params )