module CellParser
    ( parseCell, pFuncName, pOneLetterColumn, pTwoLetterColumn, parseCellCord
    ) where

import Cell
import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Control.Monad (liftM)

parseCell :: String -> CellContent
parseCell x = StringCell x

pFuncName :: GenParser Char st FuncName
pFuncName = SUMFunc <$ string "sum"
            <|> MULFunc <$ string "mul"
            <|> AVGFunc <$ string "avg"
            <?> "no function found"

numOfChar :: Char -> Int
numOfChar x = ord x - ord 'a' + 1

alphSize :: Int
alphSize = ord 'z' - ord 'a'

pOneLetterColumn :: GenParser Char st Int
pOneLetterColumn = do 
                    a <- lower 
                    return $ numOfChar a

pTwoLetterColumn :: GenParser Char st Int
pTwoLetterColumn = do 
                    a <- lower 
                    b <- lower 
                    return (numOfChar a * alphSize + numOfChar b)

-- | parse a non-negative number given a base and a parser for the digits
number :: (Integral i, Stream s m t) => Int -> ParsecT s u m Char
  -> ParsecT s u m i
number base baseDigit = do
  n <- liftM (numberValue base) (many1 baseDigit)
  seq n (return n)

-- | compute the value from a string of digits using a base
numberValue :: Integral i => Int -> String -> i
numberValue base =
  foldl (\ x -> ((fromIntegral base * x) +) . fromIntegral . digitToInt) 0


parseCellCord :: GenParser Char st CellCord
parseCellCord = do
                  a <- try pTwoLetterColumn <|> pOneLetterColumn
                  b <- number 10 digit
                  return (CellCord a b)


{-

cell = func_cell | num_cell | string_cell 
func_cell = '=' func_name ( params* )
position = ('A-Z' | 'A-Z''A-Z') [0-9]* 
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