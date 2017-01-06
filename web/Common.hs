{-# LANGUAGE OverloadedStrings #-}
module Common where

import           Cell
import qualified Data.List    as List
import           Data.Maybe
import           Data.UUID    as U
import qualified Data.UUID.V4 as U4
import           Import
import           Sheet

-- generuje tablicę wypełnioną domyślną wartością
generateEmptyValuesArray :: a -> Int -> [a]
generateEmptyValuesArray deflt n = (map (const deflt) $ take n [1,1..])

-- wypełnia tablicę pustymi ciągami znaków, tak aby tworzyła pełnoprawny prostokąt
fillTable :: [[String]] -> [[String]]
fillTable table = let maximumLength = List.maximum $ map length table in
                        map (\row -> row ++ (generateEmptyValuesArray "" (maximumLength - (length row)))) table

-- ustala dla aktualnego użytkownika klucz sesji
getSessionId :: Handler Text
getSessionId = do
    maybeSessionId <- lookupSession "session"
    if isNothing maybeSessionId
        then
            do
                random <- liftIO U4.nextRandom
                let randomSessionId = U.toText random
                setSession "session" randomSessionId
                return randomSessionId
        else return $ fromJust maybeSessionId

-- pobiera zapisany w sesji arkusz
getSavedSheetFile :: Handler (Maybe SheetFile)
getSavedSheetFile = do App _ _ _ _ appSheetStore <- getYesod
                       sessionId <- getSessionId
                       sessionSheets <- liftIO $ readTVarIO appSheetStore
                       return $ maybe Nothing Just $ lookup sessionId sessionSheets

-- usuwa arkusz z listy przechowywanej w sesji
deleteSavedSheetFile :: (Text, SheetFile) -> [(Text, SheetFile)] -> [(Text, SheetFile)]
deleteSavedSheetFile sheetToDelete sessionSheets = (List.deleteBy (\x y -> fst x == fst y) sheetToDelete sessionSheets)

-- zapisuje w sesji arkusz
saveSheetFile :: Text -> Sheet -> Handler ()
saveSheetFile filename sheetCSV = do
    App _ _ _ _ appSheetStore <- getYesod
    sessionId <- getSessionId
    let sheetSessionFile = (sessionId, SheetFile filename sheetCSV)
    liftIO . atomically $
        modifyTVar appSheetStore $ \sessionSheets -> sheetSessionFile : (deleteSavedSheetFile sheetSessionFile sessionSheets)

-- rozszerza reprezentację arkusza o wiersz w podanym miejscu
extendRawSheetByColumn :: Int -> Int -> [[String]] -> [[String]]
extendRawSheetByColumn col width rawSheet =
    if width >= col
        then (map (\row -> let splittedRow = List.splitAt col row in
                           fst splittedRow ++ [""] ++ snd splittedRow)
                        rawSheet)
        else [(List.head rawSheet) ++ (generateEmptyValuesArray "" (col - width))] ++ (List.tail rawSheet)

-- rozszerza arkusz o kolumnę w podanym miejscu
extendSheetByColumn :: Int -> Sheet -> Sheet
extendSheetByColumn column sheet@(Sheet (Dim width _) _) =
    let rawSheet = writeSheet sheet :: [[String]] in
    readSheet $ fillTable $ extendRawSheetByColumn column width rawSheet

-- rozszerza reprezentację arkusza o kolumnę w podanym miejscu
extendRawSheetByRow :: Int -> Int -> [[String]] -> [[String]]
extendRawSheetByRow row height rawSheet = if height >= row
                                then (take row rawSheet) ++ [[]] ++ (drop row rawSheet)
                                else rawSheet ++ generateEmptyValuesArray [] (row - height)

-- rozszerza arkusz o wiersz w podanym miejscu
extendSheetByRow :: Int -> Sheet -> Sheet
extendSheetByRow row sheet@(Sheet (Dim width height) _) = let rawSheet = writeSheet sheet :: [[String]] in
    readSheet $ fillTable $ extendRawSheetByRow row height rawSheet

-- reprezentuje możliwe reprezentacje arkusza
data SheetRepresentation = ProcessedSheet Sheet | RawSheet [[String]]

-- mapuje do przeprocesowanego arkusza
mapToProcessedSheet :: SheetRepresentation -> Sheet
mapToProcessedSheet (ProcessedSheet sheet) = sheet
mapToProcessedSheet (RawSheet rawSheet)    = readSheet rawSheet

-- mapuje do "gołego" arkusza
mapToRawSheet :: SheetRepresentation -> [[String]]
mapToRawSheet (ProcessedSheet sheet) = writeSheet sheet
mapToRawSheet (RawSheet rawSheet)    = rawSheet

-- zmienia wewnętrzną reprezentację jeśli zachodzi warunek
toggleSheetRepresentation :: Bool -> SheetRepresentation -> SheetRepresentation
toggleSheetRepresentation False sheetRepresentation   = sheetRepresentation
toggleSheetRepresentation True (ProcessedSheet sheet) = RawSheet (writeSheet sheet)
toggleSheetRepresentation True (RawSheet rawSheet)    = ProcessedSheet (readSheet rawSheet)

-- aplikuje funkcję tylko do "gołego" arkusza, jeśli warunek na to pozwala
applyToRawSheet :: Bool -> ([[String]] -> [[String]]) -> SheetRepresentation -> SheetRepresentation
applyToRawSheet True func (RawSheet rawSheet) = RawSheet (func rawSheet)
applyToRawSheet False func rawSheet           = rawSheet
applyToRawSheet _ _ sheetRepresentation       = sheetRepresentation

-- rozszerza arkusz, o ile tego wymaga, oraz umieszcza wartość w podaną komórkę arkusza
alterCellWithPossibleExtension :: Int -> Int -> String -> Sheet -> Sheet
alterCellWithPossibleExtension row col newValue sheet@(Sheet (Dim width height) _)
    = let sheetRepr = ProcessedSheet sheet :: SheetRepresentation
          needsWidthExtension = width < col
          needsHeightExtension = height < row
      in
        alterCell (CellCord col row) newValue $ mapToProcessedSheet
                                              $ applyToRawSheet True fillTable
                                              $ applyToRawSheet needsWidthExtension (extendRawSheetByColumn col width)
                                              $ applyToRawSheet needsHeightExtension (extendRawSheetByRow row height)
                                              $ toggleSheetRepresentation (needsHeightExtension || needsWidthExtension) sheetRepr
