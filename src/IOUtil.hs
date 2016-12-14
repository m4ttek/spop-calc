module IOUtil (
        readRawCSV,
        writeRawCSV,
        createFile,
        Separator (COMMA, SEMICOLON, COLON, TAB)
) where

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

---- implementacje funkcji publicznych -----

readRawCSV sep path = do csvRawString <- readFile path
                         return (map (splitOn (separatorToChar sep)) (splitOn "\n" csvRawString))

writeRawCSV sep path tab = writeFile path (drop 2
                                            (foldl (\x y -> x ++ (case x of "\n" -> ""; otherwise -> (separatorToChar sep)) ++ y) ""
                                              (foldl (\x y -> x ++ ["\n"] ++ y) [] tab)))

createFile path = do handle <- openFile path ReadWriteMode
                     hClose handle

---- funkcje prywatne ------

separatorToChar :: Separator -> String
separatorToChar sep = case sep of
                        COMMA     -> ","
                        SEMICOLON -> ";"
                        COLON     -> ":"
                        TAB       -> "\t"

