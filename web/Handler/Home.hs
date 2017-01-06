module Handler.Home where

import           Control.Applicative
import           Data.ByteString.Lazy.Internal as LazyByteString
import           Data.Conduit
import           Data.Conduit.Binary

import           Data.Maybe
import qualified Data.Text                     as Text
import           Import

import           Common
import           IOUtil
import           Sheet

-- kubełek na zawartość formularza
data FileForm = FileForm
    { fileInfo        :: Maybe FileInfo
    }

getFileInfo :: FileForm -> Maybe FileInfo
getFileInfo (FileForm fileInfo) = fileInfo

extractFileBytes :: Maybe FileInfo -> Handler LazyByteString.ByteString
extractFileBytes Nothing = return ""
extractFileBytes (Just fileInfo) = runResourceT $ fileSource fileInfo $$ sinkLbs

checkIfCsvFileSubmitted :: FileInfo -> Bool
checkIfCsvFileSubmitted submittedFile = fileContentType submittedFile == "text/csv"
                                        || fileContentType submittedFile == "application/csv"


getHomeR :: Handler Html
getHomeR = do
    maybeSheet <- getSavedSheetFile

    let isSubmitted = isJust maybeSheet
        isProperCSV = isSubmitted
        columns = ['A'..'Z'] :: String
        rows = [1..70] :: [Int]

    defaultLayout $ do
        setTitle "SPOP-calc"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    result <- runInputPost $ FileForm <$> iopt fileField "csv_file"
    let submission = getFileInfo result :: Maybe FileInfo
        isSubmitted = isJust submission
        isProperCSV = maybe False checkIfCsvFileSubmitted submission
        columns = ['A'..'Z'] :: String
        rows = [1..70] :: [Int]

    when isProperCSV (do
                        sheetBytes <- extractFileBytes submission
                        let sheetCSV = readSheet $ fillTable $ IOUtil.readCSVAsByteString COMMA sheetBytes
                            filename = fileName (fromJust submission)
                        saveSheetFile filename sheetCSV)
    defaultLayout $ do
        setTitle "SPOP-calc"
        $(widgetFile "homepage")

getNewR :: Handler Html
getNewR = do
    let isSubmitted = True
        isProperCSV = True
        columns = ['A'..'Z'] :: String
        rows = [1..70] :: [Int]

    saveSheetFile "new.csv" $ createEmptySheet 1 1
    defaultLayout $ do
        setTitle "SPOP-calc"
        $(widgetFile "homepage")

getDownloadR :: Handler TypedContent
getDownloadR = do
    maybeSheet <- getSavedSheetFile
    if isNothing maybeSheet
        then
            notFound
        else
            do
                let (SheetFile filename sheet) = fromJust maybeSheet :: SheetFile
                addHeader "Content-Disposition" $ Text.concat [ "attachment; filename=\"", filename, "\""]
                sendResponse (TypedContent "text/csv" $ toContent $ writeCSVAsByteString COMMA $ writeSheet sheet)
