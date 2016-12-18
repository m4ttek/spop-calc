module Sheet (
    Sheet (Sheet),
    Dim (Dim),
    getWidth, getHeight,
    createEmptySheet
) where

import           Cell
import           Data.Array

-- width height
data Dim = Dim Int Int deriving (Show, Eq)

createDim width height = Dim width height

class Measurable a where
    getWidth :: a -> Int
    getHeight :: a -> Int

instance Measurable Dim where
    getWidth (Dim width _) = width
    getHeight (Dim _ height) = height

-- arkuszt składa się z
data Sheet = Sheet Dim (Array Int (Array Int Cell)) deriving (Show, Eq)

instance Measurable Sheet where
    getWidth (Sheet x _) = getWidth x
    getHeight (Sheet x _) = getHeight x

createEmptySheet :: Int -> Int -> Sheet
createEmptySheet width height = let createRow column h = array (1,h) [(row, Cell (CellCord column row) (StringCell "") "") | row <- [1..h]]
                                    createArray w h = array (1, w) [(column, createRow column h) | column <- [1..w]]
                                in Sheet (createDim width height) (createArray width height)

