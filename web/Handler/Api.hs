{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Api where

import           Cell
import           Common
import           Data.Aeson.TH
import           Data.Maybe
import qualified Data.Text     as Text
import           Import
import           Sheet

-- "Hackson"
$(deriveJSON defaultOptions{omitNothingFields=True, unwrapUnaryRecords=False, sumEncoding = ObjectWithSingleField} ''JSONCellData)

-- reprezentacja odpowiedzi z serwera
data JSONSheetResponse = JSONSheetResponse
    {
      sheet :: JSONSheet
    }

-- "Hackson"
$(deriveJSON defaultOptions{omitNothingFields=True, unwrapUnaryRecords=False, sumEncoding = ObjectWithSingleField} ''JSONSheetResponse)

-- zwraca aktualny stan arkusza - jeśli nie został jeszcze wczytany żaden, wysyła odpowiedź o kodzie 404
getSheetjsonR :: Handler Value
getSheetjsonR = do
    maybeSheet <- getSavedSheetFile
    if isNothing maybeSheet
        then
            sendResponseStatus status404 ("{error: \"NO_FILE\"}" :: Text)
        else
            do
                let (SheetFile _ sheet) = fromJust maybeSheet :: SheetFile
                returnJson $ toJSONData sheet

-- przeprowadza aktualizację arkusza stosując przekazaną funkcję a następnie przeprowadza zapis tego formularza
updateAndSaveSheet :: (Sheet -> Sheet) -> Handler Value
updateAndSaveSheet fun = do
    maybeSheet <- getSavedSheetFile
    if isNothing maybeSheet
        then
            sendResponseStatus status404 ("{error: \"NO_FILE\"}" :: Text)
        else
            do
                let (SheetFile filename sheet) = fromJust maybeSheet :: SheetFile
                    alteredSheet = fun sheet
                saveSheetFile filename alteredSheet
                returnJson $ toJSONData alteredSheet

-- aktualizuje stan wybranej komórki arkusza - jeśli nie został jeszcze wczytany żaden, wysyła odpowiedź o kodzie 404
putUpdatesheetR :: Int -> Int -> String -> Handler Value
putUpdatesheetR row col newValue = do
    updateAndSaveSheet $ alterCellWithPossibleExtension row col (unpack $ Text.strip $ pack newValue)

putSheetrowR :: Int -> Handler Value
putSheetrowR row = do
    updateAndSaveSheet $ extendSheetByRow row

putSheetcolR :: Int -> Handler Value
putSheetcolR col = do
    updateAndSaveSheet $ extendSheetByColumn col

deleteSheetrowR :: Int -> Handler Value
deleteSheetrowR row = do updateAndSaveSheet $ removeRow row

deleteSheetcolR :: Int -> Handler Value
deleteSheetcolR col = do updateAndSaveSheet $ removeCol col