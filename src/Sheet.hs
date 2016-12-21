module Sheet (
    Sheet (Sheet),
    Dim (Dim),
    getCell,
    getCells,
    getWidth, getHeight,
    createEmptySheet,
    readSheet,
    parseSheet,
    funcCyclicCheck,
    hasCellCyclicDep,
    sheetContentWithCords,
    fixCyclicDeps
) where

import           Cell
import           CellParser

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
getCell sheet (CellCord col row) = getCells sheet !! (row - 1) !! (col - 1)

instance Measurable Sheet where
    getWidth (Sheet x _) = getWidth x
    getHeight (Sheet x _) = getHeight x

-- tworzy pusty arkusz
createEmptySheet :: Int -> Int -> Sheet
createEmptySheet width height = let createRow rowNum = [Cell (StringCell "") "" | columnNum <- [1..height]]
                                    array = [createRow rowNum | rowNum <- [1..width]]
                                in Sheet (createDim width height) array

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


funcCyclicCheck :: CellCord -> CellContent -> Sheet -> Bool
funcCyclicCheck cord (FuncCell _ params) sheet = let -- [Cell]
                                                     allSuspectCells = [getCell sheet cord | cord <- concatMap getParamCells params] -- wyciagnij cords i sklej ++
                                                     -- [Cell] (FuncCell)
                                                     funcSuspectCells = filter isFuncCell allSuspectCells
                                                     getParms (FuncCell _ params) = params
                                                     cyclicCells = filter (containsCord cord . getParms) (map getCellContent funcSuspectCells)
                                                 in not $ null cyclicCells
funcCyclicCheck _ _ _                       = error "Invalid input"

hasCellCyclicDep :: CellCord -> Sheet -> Bool
hasCellCyclicDep cord sheet = let cell = getCell sheet cord
                              in isFuncCell cell && funcCyclicCheck cord (getCellContent cell) sheet

sheetContentWithCords :: Sheet -> [[(CellCord, Cell)]]
sheetContentWithCords sheet = let cells = getCells sheet
                                  rowsWithNum = zip [1..(getHeight sheet)] cells -- [(rowNum, [rowConent])]
                                  cellWithCord rowWithNum = zipWith (\cell col-> (CellCord col (fst rowWithNum), cell)) (snd rowWithNum) [1..(getWidth sheet)]
                              in map cellWithCord rowsWithNum

fixCyclicDeps :: Sheet -> Sheet
fixCyclicDeps sheet@(Sheet dim content) = let fixCyclicDepsInRow x = if hasCellCyclicDep (fst x) sheet then
                                                                       Cell (ErrorCell CyclicDependency "cyclic dependency") (getCellOrigin (snd x))
                                                                     else snd x
                                          in Sheet dim [map fixCyclicDepsInRow row | row <- sheetContentWithCords sheet]


-- wczytuje arkusz z tablicy stringów
-- -> wiersz []
-- -> kolumna [[]]
readSheet :: [[String]] -> Sheet
readSheet cells = let height = length cells
                      width | height == 0 = 0
                            | otherwise = length $ head cells
                  in fixCyclicDeps $ Sheet (createDim width height) (parseSheet cells)
