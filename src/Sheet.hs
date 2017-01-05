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
    removeCol,
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
                                                            | rowNum <- [1..height], columnNum <- [1..width]]
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
countValue MULFunc [] = IntVal 0
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

data LineRemover = LR {
    --czy dana para (koordynant, komorka) znajduje sie w danym wierszu/kolumnie
    filterPred :: (Int -> (CellCord,Cell) -> Bool)
    --aktualizuje współrzende komórki po usunięciu wiersza/kolumny
    ,cordLowerMapper :: (Int -> (CellCord, Cell) -> (CellCord,Cell))
    -- mapuje wymiar na powstały po usunięciu
    ,dimMapper :: (Dim -> Dim)
    --funkcja mapująca paraetry funkcji po usunięciu danego wiersza
    ,mapFuncParam :: (Int -> FuncParam -> FuncParam)
    --czy usnięcie wiersza wpływa na parametr funkcji
    ,afectCell :: (Int -> FuncParam -> Bool)
     --czy parametr znajduje się w całosci na danej kolumnie/wierszu
    ,onLine :: (Int -> FuncParam -> Bool) 
     }

-- podmienia parametry żeby zawsze było (lewy górny, prawy dolny
fixRangeParam (RangeParam x y)
     = let topRow = min (getRow x) (getRow y)
           bottomRow = max (getRow x) (getRow y)
           leftCol = min (getColumn x) (getColumn y)
           rightCol = max (getColumn x) (getColumn y)
       in RangeParam (CellCord leftCol topRow) (CellCord rightCol bottomRow)


onRow row (OneCell cord) = getRow cord == row
onRow row (RangeParam x y) = getRow x == getRow y && getRow x == row
onColumn column (OneCell cord) = getColumn cord == column
onColumn column (RangeParam x y) = getColumn x == getColumn y

mapRowParam row (OneCell (CellCord x y)) 
    = let newY | row < y = y - 1
               | otherwise = y
      in OneCell (CellCord x newY)
                                         
mapRowParam row rangeParam@RangeParam{} 
    = let RangeParam (CellCord leftCol topRow) (CellCord rightCol bottomRow) = fixRangeParam rangeParam
          newTopRow | topRow > row = topRow - 1
                    | otherwise = topRow
          newBottomRow | bottomRow >= row = bottomRow - 1
                       | otherwise = bottomRow
      in RangeParam (CellCord leftCol newTopRow)
                    (CellCord rightCol newBottomRow)

mapColParam col (OneCell (CellCord x y)) 
    = let newX | col < x = x - 1
               | otherwise = x
      in OneCell (CellCord newX y)

mapColParam col rangeParam@RangeParam{} 
    = let RangeParam (CellCord leftCol topRow) (CellCord rightCol bottomRow) = fixRangeParam rangeParam
          newLeftCol | leftCol > col = leftCol - 1
                     | otherwise = leftCol
          newRightCol | rightCol >= col = rightCol - 1
                      | otherwise = rightCol
      in RangeParam (CellCord newLeftCol topRow)
                    (CellCord newRightCol bottomRow)

afterRowDimMapper dim = Dim (getWidth dim) (getHeight dim - 1)
afterColDimMapper dim = Dim (getWidth dim - 1) (getHeight dim)

rowCordMapper :: Int -> (CellCord, Cell) -> (CellCord, Cell)
rowCordMapper rowNum (cord@(CellCord col row),cell)
         | row > rowNum = (CellCord col (row - 1), cell)
         | otherwise = (cord,cell)

colCordMapper :: Int -> (CellCord, Cell) -> (CellCord, Cell)
colCordMapper colNum (cord@(CellCord col row),cell)
         | col > colNum = (CellCord (col - 1) row, cell)
         | otherwise = (cord,cell)

rowFilterPred :: Int -> (CellCord, Cell) -> Bool
rowFilterPred rowNum (cord,cell) = getRow cord /= rowNum

colFilterPred :: Int -> (CellCord, Cell) -> Bool
colFilterPred colNum (cord,cell) = getColumn cord /= colNum

rowRemover = LR {onLine = onRow
                ,filterPred = rowFilterPred
                ,mapFuncParam = mapRowParam
                ,afectCell = doesRowAfectParam
                ,cordLowerMapper = rowCordMapper
                ,dimMapper = afterRowDimMapper}

colRemover = LR {onLine = onColumn
                 ,filterPred = colFilterPred
                 ,mapFuncParam = mapColParam
                 ,afectCell = doesColAfectParam
                 ,cordLowerMapper = colCordMapper
                 ,dimMapper = afterColDimMapper} 
                 
_fixCellDeps :: Int -> LineRemover -> Cell -> Cell
_fixCellDeps line lineRemover (Cell (FuncCell funcName params value) originValue)
  = let -- pierwszy filer wyrzuc wszystkich samotnikow - jeden wiersz, jedna kolumna
        params2 = filter (\param -> not ((onLine lineRemover) line param)) params
        -- nastepnie przesuń te parametry których są związne z wierszem
        params3 = map (\param-> if (afectCell lineRemover) line param then
                                   (mapFuncParam lineRemover) line param
                                else 
                                  param) params2
        -- opakuj treść
        funcContent = FuncCell funcName params3 value
   in (Cell funcContent (modifiableCellContent funcContent))
_fixCellDeps _ _ cell = cell

--usuwa wiersz komorki
_removeLine:: Int -> LineRemover -> Sheet -> Sheet
_removeLine line lineRemover sheet@(Sheet dim content) =
  let -- odwracamy walidacje i oblczenia - arkusz po sparsowaniu
      (Sheet _ inContent) =  (clearCyclicErrors . clearFuncValues) sheet
      listCellCord = assocs inContent
      -- wyrzuc wiersz/komorke
      withoutLine = filter ((filterPred lineRemover) line) listCellCord
      -- przelicz nowy wymiar
      newDim = (dimMapper lineRemover) dim
      -- pozamieniaj współrzedne niższych komórek
      mappedCordsLower = map ((cordLowerMapper lineRemover) line) withoutLine
      -- zwin do tablicy
      mappedCordArray = array ((CellCord 1 1), CellCord (getWidth newDim) (getHeight newDim)) mappedCordsLower 
      -- pozmieniaj treść komórek (dokładniej funkcji)
      mappedFuncArray = _mapArray (_fixCellDeps line lineRemover) mappedCordArray
      -- stwórz nowa tablice
      newSheet = Sheet newDim mappedFuncArray
      -- zwaliduj i przelicz
  in findFuncValues $ fixCyclicDeps newSheet

removeRow :: Int -> Sheet -> Sheet
removeRow rowNum sheet 
          | getHeight sheet == 1 && rowNum == 1 = createEmptySheet (getWidth sheet) 1
          | rowNum > getHeight sheet            = sheet
          | otherwise                           = _removeLine rowNum rowRemover sheet

removeCol :: Int -> Sheet -> Sheet
removeCol colNum sheet 
          | getWidth sheet == 1 && colNum == 1 = createEmptySheet 1 (getHeight sheet)
          | colNum > getWidth sheet            = sheet
          | otherwise                          = _removeLine colNum colRemover sheet

