import           Test.HUnit

import           CellParserTest
import           CellTest
import           SheetTest

main :: IO Counts
main = do runTestTT (TestList [parseTests, cellTests, sheetTests])
