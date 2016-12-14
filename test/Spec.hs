import Test.HUnit
import Text.Parsec
import Cell
import CellParser
import Data.Either
import Text.Parsec.String

testFuncParsed :: (Eq a) => Parsec String () a -> String -> a -> Test
testFuncParsed parser toParse expectedValue = let parseResult = parse parser "" toParse
                                              in TestCase (assertBool "isRight" (isRight parseResult && (rights [parseResult] == [expectedValue]))) 

testFuncNotParsed :: Parsec String () FuncName -> String -> Test
testFuncNotParsed parser toParse = let parseResult = parse parser "" toParse
                                   in TestCase (assertBool "isLeft" (isLeft parseResult))

tests = TestList [TestLabel "sumFunc" (testFuncParsed pFuncName "sum" SUMFunc)
                 ,TestLabel "avgFunc" (testFuncParsed pFuncName "mul" MULFunc)
                 ,TestLabel "mulFunc" (testFuncParsed pFuncName "avg" AVGFunc)
                 ,TestLabel "failTest" (testFuncNotParsed pFuncName "adam")
                 ,TestLabel "parseCellA1" (testFuncParsed parseCellCord "a1" (CellCord 1 1))
                 ,TestLabel "parseCellZ10" (testFuncParsed parseCellCord "z10" (CellCord 26 10))]

main :: IO Counts
main = do runTestTT tests
              
