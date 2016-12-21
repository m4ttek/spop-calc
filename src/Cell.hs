module Cell (
    CellCord (CellCord),
    FuncParam (RangeParam, OneCell),
    FuncParams,
    NumberType (IntVal, DecimalVal),
    FuncName (SUMFunc, MULFunc, AVGFunc),
    ErrorType (ParseFail, CyclicDependency),
    CellContent(FuncCell, NumberCell, StringCell, ErrorCell),
    Cell (Cell),
    getCellContent,
    getCellOrigin,
    getParamCells,
    isFuncCell,
    doesParamContainsCord,
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
-- CellContent - zawartość przetworzona
-- String - napis oryginalny
data Cell = Cell CellContent String deriving (Eq, Show)

 -- funkcje
getCellContent :: Cell -> CellContent
getCellContent (Cell content _) = content

getCellOrigin :: Cell -> String
getCellOrigin (Cell _ origin) = origin

isFuncCell :: Cell -> Bool
isFuncCell (Cell (FuncCell _ _) _) = True
isFuncCell _                       = False

_start :: (CellCord -> Int) -> FuncParam -> Int
_start geter (RangeParam x y) = min (geter x) (geter y)
_end :: (CellCord -> Int) -> FuncParam -> Int
_end geter (RangeParam x y) = max (geter x) (geter y)

_in :: (CellCord -> Int) -> CellCord -> FuncParam -> Bool
_in geter cord rangeParam@(RangeParam x y) = geter cord >= _start geter rangeParam && geter cord <= _end geter rangeParam
_in geter cord (OneCell x) = geter cord == geter x

_getRange :: (CellCord -> Int) -> FuncParam -> [Int]
_getRange geter rangeParam@(RangeParam firstCord secondCord) = [(_start geter rangeParam)..(_end geter rangeParam)]



getParamCells :: FuncParam -> [CellCord]
getParamCells (OneCell cord)                    = [cord]
getParamCells rangeParam@(RangeParam firstCord secondCord)
    = [CellCord col row | col <- _getRange getColumn rangeParam, row <- _getRange getRow rangeParam]

doesParamContainsCord :: CellCord -> FuncParam -> Bool
doesParamContainsCord cord rangeParam@(RangeParam x y)
    = let insideColumn = _in getColumn cord rangeParam
          insideRow = _in getRow cord rangeParam
     in insideColumn && insideRow
doesParamContainsCord cord (OneCell x) = getColumn cord == getColumn x && getRow cord == getRow x


containsCord :: CellCord -> FuncParams -> Bool
containsCord x [] = False
containsCord x params = foldl (||) False (map (doesParamContainsCord x) params )
