module CellParserTest
    ( parseTests
    ) where


import           Cell
import           CellParser
import           Data.Either
import           Data.Ratio

import           Text.Parsec
import           Text.Parsec.String

import           Test.HUnit


testFuncParsed :: (Eq a) => Parsec String () a -> String -> a -> Test
testFuncParsed parser toParse expectedValue = let parseResult = parse parser "" toParse
                                              in TestList [TestCase (assertBool "isRight" (isRight parseResult))
                                                          ,TestCase (assertBool "equals" (rights [parseResult] == [expectedValue]))]

testFuncNotParsed :: Parsec String () a -> String -> Test
testFuncNotParsed parser toParse = let parseResult = parse parser "" toParse
                                   in TestCase (assertBool "isLeft" (isLeft parseResult))

parseTests = TestList [TestLabel "sumFunc" (testFuncParsed parseFuncName "sum" SUMFunc)
                 ,TestLabel "avgFunc" (testFuncParsed parseFuncName "mul" MULFunc)
                 ,TestLabel "mulFunc" (testFuncParsed parseFuncName "avg" AVGFunc)
                 ,TestLabel "failTest" (testFuncNotParsed parseFuncName "adam")
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
                                                                        (CellCord 2 5)])
                 ,TestLabel "parseSumFunc" (testFuncParsed parseFunc
                                                            "sum(a1:a2;a3;b4:b5)"
                                                            (FuncCell SUMFunc
                                                            [RangeParam (CellCord 1 1)
                                                                        (CellCord 1 2)
                                                            ,OneCell (CellCord 1 3)
                                                            ,RangeParam (CellCord 2 4)
                                                            (CellCord 2 5)]))
                 ,TestLabel "parseAvgFunc" (testFuncParsed parseFunc
                                                            "avg(a1:a2;a3;b4:b5)"
                                                            (FuncCell AVGFunc
                                                            [RangeParam (CellCord 1 1)
                                                                        (CellCord 1 2)
                                                            ,OneCell (CellCord 1 3)
                                                            ,RangeParam (CellCord 2 4)
                                                            (CellCord 2 5)]))
                 ,TestLabel "parseInt" (testFuncParsed parseInt
                                                      "3123"
                                                      (IntVal 3123))
                 ,TestLabel "parseDecimal" (testFuncParsed parseDecimal
                                                      "3123.323"
                                                      (DecimalVal $ approxRational (read "3123.323" :: Double) 0.0000001)) --kijowe rozwiaznie
                                                       ]


