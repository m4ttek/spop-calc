module Sheet (
    Sheet (Sheet),
    Dim (Dim),
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

createDim width height = Dim width height

class Measurable a where
    getWidth :: a -> Int
    getHeight :: a -> Int

instance Measurable Dim where
    getWidth (Dim width _) = width
    getHeight (Dim _ height) = height


type SheetContent = [[Cell]]

-- arkuszt składa się z
data Sheet = Sheet Dim SheetContent deriving (Show, Eq)

instance Measurable Sheet where
    getWidth (Sheet x _) = getWidth x
    getHeight (Sheet x _) = getHeight x

-- tworzy pusty arkusz
createEmptySheet :: Int -> Int -> Sheet
createEmptySheet width height = let createRow rowNum = [Cell (CellCord rowNum columnNum) (StringCell "") "" | columnNum <- [1..height]]
                                    array = [createRow rowNum | rowNum <- [1..width]]
                                in Sheet (createDim width height) array

sheetListToContent :: [[CellContent]] -> [[String]] -> Int -> Int -> Sheet
sheetListToContent parsedCells originCells width height
                  = let getCell tab x y = (tab !! (x - 1)) !! (y - 1)
                        createRow rowNum = [Cell (CellCord rowNum columnNum)
                                                 (getCell parsedCells columnNum rowNum)
                                                 (getCell originCells columnNum rowNum) | columnNum <- [1..height]]
                        array = [createRow rowNum | rowNum <- [1..width]]
                    in Sheet (createDim width height) array

-- zamienia [[]] na listę [[CellContent]]
parseSheet :: [[String]] -> [[CellContent]]
parseSheet = map (map parseCell)



-- wczytuje arkusz z tablicy stringów
-- -> wiersz []
-- -> kolumna [[]]
readShet :: [[String]] -> Sheet
readShet cells = let height = length cells
                     width | height == 0 = 0
                           | otherwise = length $ head cells
                 in createEmptySheet width height
