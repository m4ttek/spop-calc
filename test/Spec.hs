import           Test.HUnit

import           CellParserTest
import           CellTest

main :: IO Counts
main = do runTestTT (TestList [parseTests, cellTests])
