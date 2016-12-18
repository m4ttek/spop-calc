module CellParser
    ( parseCell,
      parseFuncName,
      parseCellCord,
      columnToNumber,
      parseRange,
      parseFuncArgs,
      parseFunc,
      parseInt,
      parseDecimal,
      parseCellWithErrors
    ) where

import           Cell
import           Data.Char
import           Data.List
import           Data.Ratio

import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.String
import           Text.Parsec.Token


parseFuncName :: GenParser Char st FuncName
parseFuncName = SUMFunc <$ string "sum"
            <|> MULFunc <$ string "mul"
            <|> AVGFunc <$ string "avg"
            <?> "function name"

-- liczba porządkowa literki - a = 1
numOfChar :: Char -> Int
numOfChar x = ord x - ord 'a' + 1

-- wartość cyfry
digitValue :: Char -> Int
digitValue x = ord x - ord '0'

-- rozmiar alfabetu
alphSize :: Int
alphSize = ord 'z' - ord 'a' + 1

-- zamieenia klumne [a-z]* na odpowiednik numeryczny
columnToNumber :: String -> Int
columnToNumber = foldl (\x y -> x * alphSize + numOfChar y) 0

-- zamienia wiersz [0-9]* na odpowiednik numeryczny
digitsToNumber :: String -> Int
digitsToNumber = foldl (\x y -> x * 10 + digitValue y) 0

-- parser współrzędnych komórki
parseCellCord :: GenParser Char st CellCord
parseCellCord = do
                  a <- many1 lower <?> "column letter"
                  b <- many1 digit <?> "row number"
                  let column = columnToNumber a
                  let row = digitsToNumber b
                  return (CellCord column row)

-- parser współrzednej postaci: a1;
parseOneCell :: GenParser Char st FuncParam
parseOneCell = do
                 cell <- parseCellCord
                 return (OneCell cell)

-- parser zakresu postacji: a1:z2;
parseRange :: GenParser Char st FuncParam
parseRange = do
               firstCell <- parseCellCord
               char ':'
               secondCell <- parseCellCord
               return (RangeParam firstCell secondCell)

-- parser argumentów funkcji
parseFuncArgs :: GenParser Char st [FuncParam]
parseFuncArgs = do char '('
                   result <- (try parseRange <|> parseOneCell) `sepBy` char ';'
                   char ')'
                   return result

-- parsuje wyrażenie (funkcje) - to co po znaku =
parseFunc :: GenParser Char st CellContent
parseFunc = do funcName <- parseFuncName
               funcParams <- parseFuncArgs
               return (FuncCell funcName funcParams)

-- parsuje liczbę całkowitą
parseInt :: GenParser Char st NumberType
parseInt = do a <- many1 digit
              return (IntVal $ digitsToNumber a)

-- parsuje liczbę zmienno przecinkową
parseDecimal :: GenParser Char st NumberType
parseDecimal = do afterDot <- many1 digit
                  char '.'
                  beforeDot <- many1 digit
                  let afterVal = digitsToNumber afterDot
                  let beforeVal = fromIntegral (digitsToNumber beforeDot) / 10 ^ length beforeDot
                  return (DecimalVal (fromIntegral afterVal + beforeVal))

-- parsuje liczbe
parseNumber :: GenParser Char st CellContent
parseNumber = do result  <- try parseDecimal <|> parseInt
                 eof -- zmuszamy parser zeby sie zakończył - nie chcemy mieć śmieci po
                 return (NumberCell result)

-- zjada wszystko
parseAll :: GenParser Char st CellContent
parseAll = do result <- many (noneOf "\n")
              return (StringCell result)

-- parsuje komórke + ignoruje bledy
parseCellWithErrors = do char '='
                         func <- parseFunc
                         return func
                      <|> try parseNumber
                      <|> parseAll

errorToStr :: ParseError -> String
errorToStr pError = intercalate " " $ map messageString $ errorMessages pError

wrapError :: ParseError -> CellContent
wrapError pError = ErrorCell ParseFail (errorToStr pError)

parseCell :: String -> CellContent
parseCell content = let parseInput x = parse parseCellWithErrors "" x
                    in either (\x -> wrapError x) id (parseInput $ map toLower content)




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
