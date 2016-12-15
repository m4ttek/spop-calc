module CellParser
    ( parseCell, pFuncName, parseCellCord, columnToNumber, parseRange, parseFuncArgs
    ) where

import Cell
import Data.Char

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Combinator


parseCell :: String -> CellContent
parseCell x = StringCell x

pFuncName :: GenParser Char st FuncName
pFuncName = SUMFunc <$ string "sum"
            <|> MULFunc <$ string "mul"
            <|> AVGFunc <$ string "avg"
            <?> "function name"

numOfChar :: Char -> Int
numOfChar x = ord x - ord 'a' + 1

digitValue :: Char -> Int
digitValue x = ord x - ord '0'

alphSize :: Int
alphSize = ord 'z' - ord 'a' + 1


columnToNumber :: String -> Int
columnToNumber letters = foldl (\x y -> x * alphSize + numOfChar y) 0 letters 

digitsToNumber :: String -> Int
digitsToNumber digits = foldl (\x y -> x * 10 + digitValue y) 0 digits


parseCellCord :: GenParser Char st CellCord
parseCellCord = do
                  a <- many1 lower <?> "column letter"
                  b <- many1 digit <?> "row number"
                  let column = columnToNumber a
                  let row = digitsToNumber b
                  return (CellCord column row)

parseOneCell :: GenParser Char st FuncParam
parseOneCell = do
                 cell <- parseCellCord
                 return (OneCell cell)

parseRange :: GenParser Char st FuncParam
parseRange = do 
               firstCell <- parseCellCord
               char ':'
               secondCell <- parseCellCord
               return (RangeParam firstCell secondCell)

parseFuncArgs :: GenParser Char st [FuncParam]
parseFuncArgs = do 
                  char '('
                  result <- ((try parseRange <|> parseOneCell) `sepBy` (char ';'))
                  char ')'
                  return result


{-

cell = func_cell | num_cell | string_cell 
func_cell = '=' func_name ( params* )
position = [A-Z]* [0-9]* 
range = position ':' position
param = position | range
params = params (';' params)*
num_cell = int_cell  | double_cell
int_cell = [0-9]*
double_cell = [0-9]* '.' [0-9]*

- nie trzeba tego defakto parsować bo to jest 
- skutek niesparsowania pozostałych
string_cell = nie num_cell i nie func_cell
-}