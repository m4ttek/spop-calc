module CellTest
    ( cellTests
    ) where


import           Cell


import           Test.HUnit


yesOneCellTest = TestCase (assertBool "contains" (containsCord (CellCord 2 2) [OneCell $ CellCord 2 2]))
noOneCellTest = TestCase (assertBool "doesntContain" (not (containsCord (CellCord 2 2) [OneCell $ CellCord 3 3])))
yesRangeCellTest = TestCase (assertBool "contains" (containsCord (CellCord 2 2) [RangeParam (CellCord 1 1) (CellCord 3 3)]))
noRangeCellTest = TestCase (assertBool "doesntContain" (not (containsCord (CellCord 3 4) [RangeParam (CellCord 1 1) (CellCord 3 3)])))

generateParamsTest = getFuncParamCords (RangeParam (CellCord 1 1) (CellCord 2 2)) ~?= [CellCord 1 1, CellCord 1 2, CellCord 2 1, CellCord 2 2]
sumFuncToNameTest = modifiableCellContent (FuncCell SUMFunc [RangeParam (CellCord 1 1) (CellCord 2 2)] Nothing) ~?= "=sum(a1:b2)"
sumFuncToNameToParamsTest = modifiableCellContent (FuncCell SUMFunc [RangeParam (CellCord 1 1) (CellCord 2 2), OneCell (CellCord 3 3)] Nothing) ~?= "=sum(a1:b2;c3)"


cellTests = TestList [TestLabel "oneCellYes" yesOneCellTest
                      ,TestLabel "oneCellNo" noOneCellTest
                      ,TestLabel "rangeYes" yesRangeCellTest
                      ,TestLabel "rangeNo" noRangeCellTest
                      ,TestLabel "generateCord" generateParamsTest
                      ,TestLabel "sumFuncToName" sumFuncToNameTest
                      ,TestLabel "sumFuncToNameParams" sumFuncToNameToParamsTest]


