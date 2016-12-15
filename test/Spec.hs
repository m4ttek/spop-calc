
import Cell
import CellParser
import Data.Either

import Text.Parsec
import Text.Parsec.String

import Test.HUnit


testFuncParsed :: (Eq a) => Parsec String () a -> String -> a -> Test
testFuncParsed parser toParse expectedValue = let parseResult = parse parser "" toParse
                                              in TestCase (assertBool "isRight" (isRight parseResult && (rights [parseResult] == [expectedValue]))) 

testFuncNotParsed :: Parsec String () a -> String -> Test
testFuncNotParsed parser toParse = let parseResult = parse parser "" toParse
                                   in TestCase (assertBool "isLeft" (isLeft parseResult))

tests = TestList [TestLabel "sumFunc" (testFuncParsed pFuncName "sum" SUMFunc)
                 ,TestLabel "avgFunc" (testFuncParsed pFuncName "mul" MULFunc)
                 ,TestLabel "mulFunc" (testFuncParsed pFuncName "avg" AVGFunc)
                 ,TestLabel "failTest" (testFuncNotParsed pFuncName "adam")
                 ,TestLabel "parseCellA1" (testFuncParsed parseCellCord "a1" (CellCord 1 1))
                 ,TestLabel "parseCellZ10" (testFuncParsed parseCellCord "z10" (CellCord 26 10))
                 ,TestLabel "parseRange" (testFuncParsed parseRange "a1:a2" (RangeParam (CellCord 1 1)
                                                                                        (CellCord 1 2)))
                 ,TestLabel "failParseRange" (testFuncNotParsed parseRange "a1:a")
                 ,TestLabel "parseFuncArgs" (testFuncParsed parseFuncArgs 
                                                            "(a1:a2;a3;b4:b5)"
                                                            [RangeParam (CellCord 1 1)
                                                                        (CellCord 1 2)
                                                            ,OneCell (CellCord 1 3)
                                                            ,RangeParam (CellCord 2 4)
                                                                        (CellCord 2 5)]
                 )]

main :: IO Counts
main = do runTestTT tests
              
