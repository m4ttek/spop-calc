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
    getFuncParamCords,
    isFuncCell,
    doesParamContainsCord,
    containsCord,
    getNumValue,
    sumNT,
    mulNT,
    divNT
) where

import           Data.Array
import           Data.Ratio

-- położenie (0,0) to lewy górny róg
-- x - kolumna
-- y - wiersze
data CellCord = CellCord Int Int deriving (Ord, Eq, Show)

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
data NumberType = IntVal Integer | DecimalVal Rational deriving (Eq, Show)

-- funkcja
data FuncName = SUMFunc | MULFunc | AVGFunc deriving (Eq, Show, Enum)

-- rodzaj błędu
data ErrorType = ParseFail | CyclicDependency  deriving (Eq, Show)

type FuncParams = [FuncParam]

-- zawartość (coś ala typ + dane)
-- FuncCell - funkcja
-- StringCont - napis
-- NumberCont - liczba
-- ErrorCell - błąd
data CellContent = FuncCell FuncName FuncParams (Maybe NumberType) | NumberCell NumberType | StringCell String | ErrorCell ErrorType String deriving (Eq, Show)

-- pełna komórka
-- CellContent - zawartość przetworzona
-- String - napis oryginalny
data Cell = Cell CellContent String deriving (Eq, Show)

 -- funkcje

-- zwraca treść (sparsowaną) komórki
getCellContent :: Cell -> CellContent
getCellContent (Cell content _) = content

-- zwraca treść komórki wpisaną przez użytkownika
getCellOrigin :: Cell -> String
getCellOrigin (Cell _ origin) = origin

-- sprawdza czy komórka zawiera funkcję
isFuncCell :: Cell -> Bool
isFuncCell (Cell FuncCell{} _) = True
isFuncCell _                   = False

-- funkcje pomocnicze
_start :: (CellCord -> Int) -> FuncParam -> Int
_start geter (RangeParam x y) = min (geter x) (geter y)
_end :: (CellCord -> Int) -> FuncParam -> Int
_end geter (RangeParam x y) = max (geter x) (geter y)

_in :: (CellCord -> Int) -> CellCord -> FuncParam -> Bool
_in geter cord rangeParam@(RangeParam x y) = geter cord >= _start geter rangeParam && geter cord <= _end geter rangeParam
_in geter cord (OneCell x) = geter cord == geter x

_getRange :: (CellCord -> Int) -> FuncParam -> [Int]
_getRange geter rangeParam@(RangeParam firstCord secondCord) = [(_start geter rangeParam)..(_end geter rangeParam)]


-- zwraca adresy komórek odpowiadają parametrom funkcji
getFuncParamCords :: FuncParam -> [CellCord]
getFuncParamCords (OneCell cord)                    = [cord]
getFuncParamCords rangeParam@(RangeParam firstCord secondCord)
    = [CellCord col row | col <- _getRange getColumn rangeParam, row <- _getRange getRow rangeParam]

-- sprawdza czy komórka jest pośród parametru funkcji
doesParamContainsCord :: CellCord -> FuncParam -> Bool
doesParamContainsCord cord rangeParam@(RangeParam x y)
    = let insideColumn = _in getColumn cord rangeParam
          insideRow = _in getRow cord rangeParam
     in insideColumn && insideRow
doesParamContainsCord cord (OneCell x) = getColumn cord == getColumn x && getRow cord == getRow x


-- jak wyżej tylko dla wszystkich parametrów
containsCord :: CellCord -> FuncParams -> Bool
containsCord x []     = False
containsCord x params = any (doesParamContainsCord x) params


-- Implementacja indeksowania po CellCordzie przy pomocy powyższych funkcji
instance Ix CellCord where
    -- The list of values in the subrange defined by a bounding pair.
    range (cordA,cordB) = getFuncParamCords (RangeParam cordA cordB)
    -- Returns True the given subscript lies in the range defined the bounding pair
    inRange (cordA,cordB) toCheck = doesParamContainsCord toCheck (RangeParam cordA cordB)
    -- The position of a subscript in the subrange.
    index (cordA,cordB) toFind = let rangeParam = RangeParam cordA cordB
                                     startCol = _start getColumn rangeParam
                                     endCol = _end getColumn rangeParam
                                     startRow = _start getRow rangeParam
                                     columns = endCol - startCol + 1
                                 in ((getRow toFind) - startRow) * columns + getColumn toFind - 1 -- [0, n-1]


-- typ do operacji numeryczny - prezentowanie wartości, wyliczanie wartości funkcji
class NumericValue a where
    getNumValue :: a -> Maybe NumberType

instance NumericValue CellContent where
    getNumValue (FuncCell _ _ maybeValue) = maybeValue
    getNumValue (NumberCell val)          = Just val
    getNumValue (StringCell _)            = Just $ IntVal 0
    getNumValue (ErrorCell _ _)           = Just $ IntVal 0

instance NumericValue Cell where
    getNumValue (Cell cellContent _) = getNumValue cellContent


sumNT :: NumberType -> NumberType -> NumberType
sumNT (IntVal x) (IntVal y) = IntVal $ x + y
sumNT (IntVal x) (DecimalVal y) = DecimalVal $ (x * denominator y + numerator y) % denominator y
sumNT x@(DecimalVal _) y@(IntVal _)  = sumNT x y
sumNT (DecimalVal x) (DecimalVal y) = DecimalVal $ (numerator x * denominator y + numerator y * denominator x)
                                    % (denominator x * denominator y)

mulNT :: NumberType -> NumberType -> NumberType
mulNT (IntVal x) (IntVal y) = IntVal $ x * y
mulNT (IntVal x) (DecimalVal y) = DecimalVal $ (x * numerator y) % denominator y
mulNT x@(DecimalVal _) y@(IntVal _)  = mulNT x y
mulNT (DecimalVal x) (DecimalVal y) = DecimalVal $ (numerator x * numerator y) % (denominator x * denominator y)

divNT :: NumberType -> Int -> NumberType
divNT (IntVal x) y = let yy = toInteger y
                     in if x `mod` yy == (0 :: Integer) then
                          IntVal $ x `div` yy
                        else
                          DecimalVal $ x % yy
divNT (DecimalVal x) y = DecimalVal $ numerator x % (denominator x * toInteger y)

