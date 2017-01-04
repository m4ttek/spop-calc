module Sheet (
    Sheet (Sheet),
    Dim (Dim),
    getCell,
    getCells,
    getWidth, getHeight,
    createEmptySheet,
    readSheet,
    writeSheet,
    parseSheet,
    JSONCellData (JSONCellData),
    JSONSheet,
    toJSONData,
    alterCell,
    removeRow,
    fixCyclicDeps,
    findFuncValues
) where

import           Cell
import           CellParser
import           ShowRational

import           Data.Array
import           Data.Maybe


-- width height
data Dim = Dim Int Int deriving (Show, Eq)

createDim = Dim

class Measurable a where
    getWidth :: a -> Int
    getHeight :: a -> Int

instance Measurable Dim where
    getWidth (Dim width _) = width
    getHeight (Dim _ height) = height

type SheetContent = Array CellCord Cell

-- arkuszt składa się z
data Sheet = Sheet Dim SheetContent deriving (Show, Eq)

getCells :: Sheet -> SheetContent
getCells (Sheet _ cells) = cells

getCell :: Sheet -> CellCord -> Cell
getCell sheet cord@(CellCord col row) = if col > getWidth sheet || col < 1 || row > getHeight sheet || row < 1
                                   then
                                     Cell (StringCell "") ""
                                   else
                                     getCells sheet ! cord

instance Measurable Sheet where
    getWidth (Sheet x _) = getWidth x
    getHeight (Sheet x _) = getHeight x

-- tworzy pusty arkusz
createEmptySheet :: Int -> Int -> Sheet
createEmptySheet width height = let bounds = (CellCord 1 1, CellCord width height)
                                    content = array bounds [(CellCord columnNum rowNum, Cell (StringCell "") "")
                                                            | rowNum <- [1..width], columnNum <- [1..height]]
                                in Sheet (createDim width height) content


_mapArray :: (Ix ix) => (a -> a) -> Array ix a -> Array ix a
_mapArray f arr = array (bounds arr) [(fst indWithElem, f $ snd indWithElem)| indWithElem <- assocs arr]
_mapArrayWithIdx :: (Ix ix) => (ix -> a -> a) -> Array ix a -> Array ix a
_mapArrayWithIdx f arr =  array (bounds arr) [(fst indWithElem, f (fst indWithElem) (snd indWithElem)) | indWithElem <- assocs arr]

-- mapuje komórki arkusza przy pomocy funkcji
mapCells :: (Cell -> Cell) -> Sheet -> Sheet
mapCells mappingFunc sheet@(Sheet dim content) = let mappedContent = _mapArray mappingFunc content
                                                 in Sheet dim mappedContent

-- mapuje komórki arkusza przy pomocy funkcji funkcja poza komorką przyjmuje koordynant tej komórki
mapCellWithCord :: (CellCord -> Cell -> Cell) -> Sheet -> Sheet
mapCellWithCord mappingFunc sheet@(Sheet dim content) = let mappedContent = _mapArrayWithIdx mappingFunc content
                                                        in Sheet dim mappedContent

-- zamienia [[]] na listę [[CellContent]]
parseSheet :: Dim -> [[String]] -> SheetContent
parseSheet dim@(Dim width height) rawCont
    = let bounds = (CellCord 1 1, CellCord width height)
          rowsWithNum = zip [1..height] rawCont -- [(rowNum, [rowConent])]
          cellWithCord rowWithNum = zipWith (\rawCell col-> (CellCord col (fst rowWithNum), parseCell rawCell)) (snd rowWithNum) [1..width]
          content = concat $ map cellWithCord rowsWithNum
      in array bounds content


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


-- zamienia komórki o cyklicznych zależnościach na błędne
fixCyclicDeps :: Sheet -> Sheet
fixCyclicDeps sheet@(Sheet dim content) = let fixCyclicDepsCell cord cell = if hasCellCyclicDep cord sheet then
                                                                           Cell (ErrorCell CyclicDependency "cyclic dependency") (getCellOrigin cell)
                                                                        else cell
                                          in mapCellWithCord fixCyclicDepsCell sheet



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
findValuesWherePossible sheet@(Sheet dim content) = Sheet dim (_mapArray (\cell -> findValuesForCell cell sheet) content)

-- wylicza wartości komórek funkcyjnych
findFuncValues :: Sheet -> Sheet
findFuncValues sheet@(Sheet dim content) = let allCells = elems content -- złącz wszystkie komórki w liste
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
                      dim = createDim width height
                  in findFuncValues $ fixCyclicDeps $ Sheet dim (parseSheet dim cells)

-- zapisuje arkusz do tablicy stringów
-- -> wiersz []
-- -> kolumna [[]]
writeSheet :: Sheet -> [[String]]
writeSheet sheet@(Sheet (Dim width height) cells) = (map
                                                      (\y -> (map
                                                               (\x -> getCellOrigin $ getCell sheet $ CellCord x y)
                                                               [1..width]))
                                                      [1..height])

--typ reprezentujacy formę komórki przesyłaną JSONem
data JSONCellData = JSONCellData String String deriving (Show, Eq)
--typ rezprezentujący formę arkusza przesyłaną JSONem
type JSONSheet = [[JSONCellData]]



-- transformuje komórke do znośnej formy, zakłada się, że wszystkie komórki mają wyliczone wartości
toJSONData :: Sheet -> JSONSheet
toJSONData sheet@(Sheet dim content) = let transformContent (StringCell value) = value
                                           transformContent (NumberCell (IntVal val)) = show val
                                           transformContent (NumberCell (DecimalVal val)) = showRational (Just 8) val
                                           transformContent (FuncCell _ _ Nothing) = error "invalid cell"
                                           transformContent (FuncCell _ _ (Just (IntVal val))) = show val
                                           transformContent (FuncCell _ _ (Just (DecimalVal val))) = showRational (Just 8) val
                                           transformContent (ErrorCell errorType _) = "ERR: " ++ show errorType
                                           transformCell (Cell content origin) = JSONCellData (transformContent content) origin
                                           cellsCords = [[CellCord col row | col <- [1..(getWidth dim)]] | row <- [1..(getHeight dim)]]
                                           getCellAndTransform cord = transformCell (getCell sheet cord)
                                           mapCordToCont = map (map getCellAndTransform) cellsCords
                                        in mapCordToCont


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
alterCell :: CellCord -> String -> Sheet -> Sheet
alterCell newCord newValue sheet@(Sheet dim content) =
    let -- odwracamy walidacje i oblczenia - arkusz po sparsowaniu
        (Sheet _ inContent) =  (clearCyclicErrors . clearFuncValues) sheet
        -- podmina w tablicy z koordynatami
        swappedContent = Sheet dim (inContent // [(newCord, parseCell newValue)])
    in findFuncValues $ fixCyclicDeps swappedContent

doesRowAfectParam :: Int -> FuncParam -> Bool
doesRowAfectParam row rangeParam@(RangeParam x y)
    = max (getRow x) (getRow y) >= row
doesRowAfectParam row (OneCell cord) = getRow cord >= row
    
doesColAfectParam :: Int -> FuncParam -> Bool
doesColAfectParam col rangeParam@(RangeParam x y) 
    = max (getColumn x) (getColumn y) >= col
doesColAfectParam col (OneCell cord) = getColumn cord >= col

_fixRowCelDeps row (Cell (FuncCell funcName params value) originValue)
  = let oneRow (OneCell _) = True
        oneRow (RangeParam x y) = getRow x == getRow y
        -- pierwszy filer wyrzuc wszystkich samotnikow - jeden wiersz
        params2 = filter (\param-> True) params
        mapParam (RangeParam x y) = let topRow = min (getRow x) (getRow y)
                                        bottomRow = max (getRow x) (getRow y)
                                        leftCol = min (getColumn x) (getColumn y)
                                        rightCol = max (getColumn x) (getColumn y)
                                        newTopRow | topRow > row = topRow - 1
                                                  | otherwise = topRow
                                        newBottomRow | bottomRow > row = bottomRow - 1
                                                     | otherwise = bottomRow
                                    in RangeParam (CellCord leftCol newTopRow)
                                                  (CellCord rightCol newBottomRow)
        -- nastepnie przemuj te parametry które zachaczają o wiersz
        params3 = map (\param-> if doesRowAfectParam row param then
                                  mapParam param
                                else 
                                  param) params2
        funcContent = FuncCell funcName params3 value
   in (Cell funcContent (modifiableCellContent funcContent))
_fixRowCelDeps row cell = cell

--usuwa wiersz komorki
removeRow :: Int -> Sheet -> Sheet
removeRow rowNum sheet@(Sheet dim content) =
  let -- odwracamy walidacje i oblczenia - arkusz po sparsowaniu
      (Sheet _ inContent) =  (clearCyclicErrors . clearFuncValues) sheet
      listCellCord = assocs inContent
      -- wyrzuc wiersz
      withoutRow = filter (\(cord,_) -> getRow cord /= rowNum) listCellCord
      -- pozamieniaj współrzedne niższych
      mappedCordsLower = map (\(cord@(CellCord col row),cell) -> 
                                if row > rowNum then
                                  (CellCord col (row - 1), cell)
                                else
                                  (cord,cell)) withoutRow
      -- zwin do tablicy
      mappedCordArray = array ((CellCord 1 1), CellCord (getWidth newDim) (getHeight newDim)) mappedCordsLower 
      -- pozmieniaj treść komórek
      mappedFuncArray = _mapArray (_fixRowCelDeps rowNum) mappedCordArray
      -- przelicz nowy wymiar
      newDim = Dim (getWidth dim) (getHeight dim - 1)
      -- stwórz nowa tablice
      newSheet = Sheet newDim mappedFuncArray
  in findFuncValues $ fixCyclicDeps newSheet


