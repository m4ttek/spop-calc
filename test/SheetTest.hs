module SheetTest
    ( sheetTests
    ) where


import           Cell
import           Sheet


import           Test.HUnit

isCyclicError (Cell (ErrorCell CyclicDependency _) _) = True
isCyclicError _                                       = False

simpleSheet = readSheet [["=SUM(B1)","=SUM(A1)"],["0", "0"], ["0", "0"]]

cyclicDepenDetectTest cord = TestCase $ assertBool "hasCyclicDep" (isCyclicError $ getCell simpleSheet cord)

sheetTests = TestList [TestLabel "cyclicDepenDetectTest" (cyclicDepenDetectTest (CellCord 1 1))
                     , TestLabel "cyclicDepenDetectTest" (cyclicDepenDetectTest (CellCord 2 1))]


--[["=SUM(A2:B3)","=AVG(A2:B3)"],["2", "2"], ["2", "2"]]
