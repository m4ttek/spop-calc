module Sheet (
    Sheet (Sheet),
    Dim (Dim),
    getCell,
    getCells,
    getWidth, getHeight,
    createEmptySheet,
    readSheet,
    parseSheet,
    JSONCellData (JSONCellData),
    JSONSheet,
    toJSONData,
    alterCell
) where

import           Cell
import           CellParser
import           ShowRational

import           Data.Maybe
import           Debug.Trace


-- width height
data Dim = Dim Int Int deriving (Show, Eq)

createDim = Dim

class Measurable a where
    getWidth :: a -> Int
    getHeight :: a -> Int

instance Measurable Dim where
    getWidth (Dim width _) = width
    getHeight (Dim _ height) = height


type SheetContent = [[Cell]]

-- arkuszt składa się z
data Sheet = Sheet Dim SheetContent deriving (Show, Eq)

getCells :: Sheet -> SheetContent
getCells (Sheet _ cells) = cells

getCell :: Sheet -> CellCord -> Cell
getCell sheet (CellCord col row) = if col > getWidth sheet || col < 1 || row > getHeight sheet || row < 1
                                   then
                                     Cell (StringCell "") ""
                                   else
                                     getCells sheet !! (row - 1) !! (col - 1)

instance Measurable Sheet where
    getWidth (Sheet x _) = getWidth x
    getHeight (Sheet x _) = getHeight x

-- tworzy pusty arkusz
createEmptySheet :: Int -> Int -> Sheet
createEmptySheet width height = let createRow rowNum = [Cell (StringCell "") "" | columnNum <- [1..height]]
                                    array = [createRow rowNum | rowNum <- [1..width]]
                                in Sheet (createDim width height) array


-- funkcja generyczna aplikujaca mapowanie na liste list
mapTwoDim ::  (a -> a) -> [[a]] -> [[a]]
mapTwoDim mappingFunc content = let mapRow = map mappingFunc
                                in map mapRow content

-- mapuje komórki arkusza przy pomocy funkcji
mapCells :: (Cell -> Cell) -> Sheet -> Sheet
mapCells mappingFunc sheet@(Sheet dim content) = let mappedContent = mapTwoDim mappingFunc content
                                                 in Sheet dim mappedContent

-- pobiera:
-- sparsowane komórki
-- oryginalną listę
-- rozmiar arkusza
-- zwraca:
-- arkusz
sheetListToContent :: [[CellContent]] -> [[String]] -> Int -> Int -> Sheet
sheetListToContent parsedCells originCells width height
                  = let getCell tab x y = (tab !! (y - 1) !! (x - 1))
                        createRow rowNum = [Cell (getCell parsedCells columnNum rowNum)
                                                 (getCell originCells columnNum rowNum) | columnNum <- [1..height]]
                        array = [createRow rowNum | rowNum <- [1..width]]
                    in Sheet (createDim width height) array

-- zamienia [[]] na listę [[CellContent]]
parseSheet :: [[String]] -> [[Cell]]
parseSheet = map (map parseCell)


-- pobiera komórki składające się na argumenty funkcji
getFuncParamsCells :: Sheet -> FuncParams -> [Cell]
getFuncParamsCells sheet funcParams = [getCell sheet cord | cord <- concatMap getFuncParamCords funcParams]

-- sprawdza czy podana komórka zawierająca funkcję
-- ma cykliczną zależność
funcCyclicCheck :: CellCord -> CellContent -> Sheet -> Bool
funcCyclicCheck cord (FuncCell _ funcParams _) sheet = let -- [Cell]
                                                     allSuspectCells = getFuncParamsCells sheet funcParams -- wyciagnij cords i sklej ++
                                                     -- [Cell] (FuncCell)
                                                     funcSuspectCells = filter isFuncCell allSuspectCells
                                                     getParms (FuncCell _ params _) = params
                                                     cyclicCells = filter (containsCord cord . getParms) (map getCellContent funcSuspectCells)
                                                 in not $ null cyclicCells
funcCyclicCheck _ _ _                       = error "Invalid input"

-- sprawdza czy komórka jest funkcyjna i ma cykliczną zależność
hasCellCyclicDep :: CellCord -> Sheet -> Bool
hasCellCyclicDep cord sheet = let cell = getCell sheet cord
                              in isFuncCell cell && funcCyclicCheck cord (getCellContent cell) sheet

-- zwraca treść arkusza wraz z odpowiadającymi pozycjami
sheetContentWithCords :: Sheet -> [[(CellCord, Cell)]]
sheetContentWithCords sheet = let cells = getCells sheet
                                  rowsWithNum = zip [1..(getHeight sheet)] cells -- [(rowNum, [rowConent])]
                                  cellWithCord rowWithNum = zipWith (\cell col-> (CellCord col (fst rowWithNum), cell)) (snd rowWithNum) [1..(getWidth sheet)]
                              in map cellWithCord rowsWithNum

-- zamienia komórki o cyklicznych zależnościach na błędne
fixCyclicDeps :: Sheet -> Sheet
fixCyclicDeps sheet@(Sheet dim content) = let fixCyclicDepsInRow x = if hasCellCyclicDep (fst x) sheet then
                                                                       Cell (ErrorCell CyclicDependency "cyclic dependency") (getCellOrigin (snd x))
                                                                     else snd x
                                          in Sheet dim [map fixCyclicDepsInRow row | row <- sheetContentWithCords sheet]



-- znajduje wartość dla danej funkcji i parametrów
countValue :: FuncName -> [NumberType] -> NumberType
countValue SUMFunc numbers = foldl sumNT (IntVal 0) numbers
countValue MULFunc numbers = foldl mulNT (IntVal 1) numbers
countValue AVGFunc numbers = divNT (countValue SUMFunc numbers) (length numbers)

-- znajduje wartość dla funkcji
findValuesForCell :: Cell -> Sheet -> Cell
findValuesForCell cell@(Cell (FuncCell _ _ (Just numValue)) _) sheet = cell
findValuesForCell cell@(Cell (FuncCell funcName params Nothing) origin) sheet
  = let funcParamsCells = getFuncParamsCells sheet params
        maybeCellValues = map getNumValue funcParamsCells
        allJust = all isJust maybeCellValues
    in if allJust then
         Cell (FuncCell funcName params (Just (countValue funcName (catMaybes maybeCellValues)))) origin
       else
         cell

findValuesForCell cell _ = cell

-- wylicza wartości funkcji wszędzie tam, gdzie można je obliczyć
findValuesWherePossible sheet@(Sheet dim content) = mapCells (\cell -> findValuesForCell cell sheet) sheet

-- wylicza wartości komórek funkcyjnych
findFuncValues :: Sheet -> Sheet
findFuncValues sheet@(Sheet dim content) = let allCells = concat content -- złącz wszystkie komórki w liste
                                               funcCells = filter isFuncCell allCells
                                               allJust = all isJust (map getNumValue funcCells)
                                              -- jak wszystkie są osiągalne do zwróc arkusz bez zmian
                                           in if allJust then
                                                 sheet
                                              -- jak nie to napraw i spróbuj dalej wyliczać (funkcje od funkcji)
                                              else
                                                 findFuncValues $ findValuesWherePossible sheet


-- wczytuje arkusz z tablicy stringów
-- -> wiersz []
-- -> kolumna [[]]
readSheet :: [[String]] -> Sheet
readSheet cells = let height = length cells
                      width | height == 0 = 0
                            | otherwise = length $ head cells
                  in findFuncValues $ fixCyclicDeps $ Sheet (createDim width height) (parseSheet cells)


--typ reprezentujacy formę komórki przesyłaną JSONem
data JSONCellData = JSONCellData String String deriving (Show, Eq)
--typ rezprezentujący formę arkusza przesyłaną JSONem
type JSONSheet = [[JSONCellData]]



-- transformuje komórke do znośnej formy, zakłada się, że wszystkie komórki mają wyliczone wartości
toJSONData :: Sheet -> JSONSheet
toJSONData (Sheet dim content) = let transformContent (StringCell value) = value
                                     transformContent (NumberCell (IntVal val)) = show val
                                     transformContent (NumberCell (DecimalVal val)) = showRational (Just 8) val
                                     transformContent (FuncCell _ _ Nothing) = error "invalid cell"
                                     transformContent (FuncCell _ _ (Just (IntVal val))) = show val
                                     transformContent (FuncCell _ _ (Just (DecimalVal val))) = showRational (Just 8) val
                                     transformContent (ErrorCell errorType _) = "ERR: " ++ show errorType
                                     transformCell (Cell content origin) = JSONCellData (transformContent content) origin
                                     transformRow = map transformCell
                                 in map transformRow content





_clearFuncValue (FuncCell funcName params val) = FuncCell funcName params Nothing
_clearFuncValue _ = error "not function cell"

-- czysci obliczone wartości arkusza
clearFuncValues sheet@(Sheet dim content) = let clearFuncCell (Cell funCell@FuncCell{} originValue) = Cell (_clearFuncValue funCell) originValue
                                                clearFuncCell cell = cell
                                            in mapCells clearFuncCell sheet


-- czysci komórki które mogłby się odcyklicznić
clearCyclicErrors sheet@(Sheet dim content) = let clearCycErrorCell (Cell (ErrorCell CyclicDependency _) originValue) = parseCell originValue
                                                  clearCycErrorCell cell = cell
                                             in mapCells clearCycErrorCell sheet


-- zamiana komórki arkusza
alterCell :: Sheet -> CellCord -> String -> Sheet
alterCell sheet@(Sheet dim content) newCord newValue =
    let -- odwracamy walidacje i oblczenia - arkusz po sparsowaniu
        inSheet =  (clearCyclicErrors . clearFuncValues) sheet
        -- dopisuje koordynaty
        inSheetWithCord = sheetContentWithCords inSheet
        -- (CellCord, Cell) -> (CellCord, Cell)
        trySwapCell (cord, cell) = if cord == newCord then
                                     (cord, parseCell newValue)
                                   else
                                     (cord, cell)
        -- podmina w tablicy z koordynatami
        swappedContent = mapTwoDim trySwapCell inSheetWithCord
        -- ekstrat wartości
        newContent = map (map snd) swappedContent
    in findFuncValues $ fixCyclicDeps $ Sheet dim newContent
