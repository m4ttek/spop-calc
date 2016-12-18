import           Test.HUnit

import           CellParserTest

main :: IO Counts
main = do runTestTT (TestList [parseTests])
