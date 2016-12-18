module CellParser
    ( parseCell,
      parseFuncName,
      parseCellCord,
      columnToNumber,
      parseRange,
      parseFuncArgs,
      parseFunc,
      parseInt,
      parseDecimal
    ) where

import           Cell
import           Data.Char
import           Data.Ratio

import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.String
import           Text.Parsec.Token


parseCell :: String -> CellContent
parseCell = StringCell

parseFuncName :: GenParser Char st FuncName
parseFuncName = SUMFunc <$ string "sum"
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
columnToNumber = foldl (\x y -> x * alphSize + numOfChar y) 0

digitsToNumber :: String -> Int
digitsToNumber = foldl (\x y -> x * 10 + digitValue y) 0


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
parseFuncArgs = do char '('
                   result <- (try parseRange <|> parseOneCell) `sepBy` char ';'
                   char ')'
                   return result

-- parsuje wyrażenie -funkcje po znaku =
parseFunc :: GenParser Char st CellContent
parseFunc = do funcName <- parseFuncName
               funcParams <- parseFuncArgs
               return (FuncCell funcName funcParams)


parseInt :: GenParser Char st NumberType
parseInt = do a <- many1 digit
              return (IntVal $ digitsToNumber a)

parseDecimal :: GenParser Char st NumberType
parseDecimal = do afterDot <- many1 digit
                  char '.'
                  beforeDot <- many1 digit
                  let afterVal = digitsToNumber afterDot
                  let beforeVal = (fromIntegral $ digitsToNumber beforeDot) / 10 ^ length beforeDot
                  return (DecimalVal (fromIntegral afterVal + beforeVal))



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
