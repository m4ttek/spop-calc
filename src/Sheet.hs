module Sheet (
    Sheet (Sheet),
    Dim (Dim),
    getCells,
    getWidth, getHeight,
    createEmptySheet,
    readShet,
    parseSheet
) where

import           Cell
import           CellParser
--import           Data.Array

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
getCell sheet (CellCord col row) = getCells sheet !! (col - 1) !! (row - 1)

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
                  = let getCell tab x y = (tab !! (x - 1)) !! (y - 1)
                        createRow rowNum = [Cell (getCell parsedCells columnNum rowNum)
                                                 (getCell originCells columnNum rowNum) | columnNum <- [1..height]]
                        array = [createRow rowNum | rowNum <- [1..width]]
                    in Sheet (createDim width height) array

-- zamienia [[]] na listę [[CellContent]]
parseSheet :: [[String]] -> [[Cell]]
parseSheet = map (map parseCell)


funcCyclicCheck :: CellContent -> Sheet -> Bool
funcCyclicCheck (FuncCell _ params) sheet = False
funcCyclicCheck _ _                       = error "Invalid input"

hasCellCyclicDep :: CellCord -> Sheet -> Bool
hasCellCyclicDep cord sheet = let cell = getCell sheet cord
                              in isFuncCell cell && funcCyclicCheck (getCellContent cell) sheet

fixCyclicDeps :: Sheet -> Sheet
fixCyclicDeps x = x


-- wczytuje arkusz z tablicy stringów
-- -> wiersz []
-- -> kolumna [[]]
readShet :: [[String]] -> Sheet
readShet cells = let height = length cells
                     width | height == 0 = 0
                           | otherwise = length $ head cells
                 in Sheet (createDim width height) (parseSheet cells)
