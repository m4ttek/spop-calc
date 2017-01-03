{-# LANGUAGE OverloadedStrings #-}

module IOUtil (
        readRawCSV,
        writeRawCSV,
        createFile,
        readCSVAsByteString,
        writeCSVAsByteString,
        Separator (COMMA, SEMICOLON, COLON, TAB)
) where

import qualified Data.ByteString.Lazy.Char8    as LazyChar
import           Data.ByteString.Lazy.Internal
import           Data.List
import           Data.List.Split
import           System.IO

-- separator komórek w pliku CSV
data Separator = COMMA | SEMICOLON | COLON | TAB

-- czyta plik csv z podanej ścieżki
-- @param ścieżka
-- @param separator
-- @return tablica wartości komórek
readRawCSV :: Separator -> String -> IO [[String]]

-- zapisuje plik csv do podanej ścieżki
-- @param tablica do zapisania
writeRawCSV :: Separator -> String -> [[String]] -> IO ()

-- tworzy nowy plik pod daną ścieżką, funkcja przydatna dla walidacji ścieżki / uprawnień
-- @param ścieżka
createFile :: String -> IO ()

-- wczytuje bajty, przetwarzając go na wewnętrzną reprezentację pliku CSV
readCSVAsByteString :: Separator -> ByteString -> [[String]]

-- zapisuje reprezentację pliku CSV jako bajty
writeCSVAsByteString :: Separator -> [[String]] -> ByteString


---- implementacje funkcji publicznych -----

readRawCSV sep path = do csvRawString <- readFile path
                         return $ map (splitOn (separatorToChar sep)) (splitOn "\n" csvRawString)

writeRawCSV sep path tab = writeFile path (tail
                                            (foldr (\x y -> x ++
                                                              case x of "\n" -> ""
                                                                        _ -> separatorToChar sep
                                                              ++ y) ""
                                              (foldl (\x y -> x ++ ["\n"] ++ y) [] tab)))

createFile path = do handle <- openFile path ReadWriteMode
                     hClose handle

readCSVAsByteString sep byteString = map (splitOn (separatorToChar sep)) (splitOn "\n" (LazyChar.unpack byteString))

writeCSVAsByteString sep tab = LazyChar.pack (tail
                                                (foldr (\x y -> x ++
                                                                case x of "\n" -> ""
                                                                          _ -> separatorToChar sep
                                                                ++ y) ""
                                                        (foldl (\x y -> x ++ ["\n"] ++ y) [] tab)))

---- funkcje prywatne ------

separatorToChar :: Separator -> String
separatorToChar sep = case sep of
                        COMMA     -> ","
                        SEMICOLON -> ";"
                        COLON     -> ":"
                        TAB       -> "\t"

