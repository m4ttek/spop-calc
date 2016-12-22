module SheetTest
    ( sheetTests
    ) where


import           Cell
import           Sheet


import           Test.HUnit

isCyclicError (Cell (ErrorCell CyclicDependency _) _) = True
isCyclicError _                                       = False

selfCyclicSheet = [["=SUM(A1)"]]
cyclicSimpleSheet = [["=SUM(B1)","=SUM(A1)"],["0", "0"], ["0", "0"]]



cyclicDepenDetectTest cord notParsedCells = TestCase $ assertBool "hasCyclicDep" (isCyclicError $ getCell (readSheet notParsedCells) cord)

cyclicTest = TestList [TestLabel "selfCyclicTest" (cyclicDepenDetectTest (CellCord 1 1) selfCyclicSheet)
                     , TestLabel "cyclicDepenDetectTest1" (cyclicDepenDetectTest (CellCord 1 1) cyclicSimpleSheet)
                     , TestLabel "cyclicDepenDetectTest2" (cyclicDepenDetectTest (CellCord 2 1) cyclicSimpleSheet)]

json :: String -> String -> JSONCellData
json = JSONCellData

funcValuesSheet = [["=SUM(A2:B3)","=AVG(A2:B3)"],["2", "2"], ["2", "2"]]
funcValuesTest = TestList [TestLabel
                                "funcValuesTest"
                                ((toJSONData . readSheet) funcValuesSheet
                                 ~?=
                                 [[json "8" "=SUM(A2:B3)", json "2" "=AVG(A2:B3)"],
                                  [json "2" "2", json "2" "2"],
                                  [json "2" "2", json "2" "2"]])]



sheetTests = TestList [cyclicTest
                      ,funcValuesTest]
--[["=SUM(A2:B3)","=AVG(A2:B3)"],["2", "2"], ["2", "2"]]