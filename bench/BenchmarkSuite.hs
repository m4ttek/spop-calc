import           Cell
import           Criterion.Main
import           Sheet

simpleSheet = [["0" | _ <- [1..100]] | _ <- [1..100]]

deepSheet = [["1" | _ <- [1..100]]
            ,["=SUM(A1:CV1)" | _ <- [1..100]]
            ,["=SUM(A1:CV2)" | _ <- [1..100]]
            ,["=SUM(A1:CV3)" | _ <- [1..100]]
            ,["=SUM(A1:CV4)" | _ <- [1..100]]
            ,["=SUM(A1:CV5)" | _ <- [1..100]]
            ,["=SUM(A1:CV6)" | _ <- [1..100]]
            ,["=SUM(A1:CV7)" | _ <- [1..100]]
            ,["=SUM(A1:CV8)" | _ <- [1..100]]
            ,["=SUM(A1:CV9)" | _ <- [1..100]]]


readSheetBench = length . toJSONData . readSheet

readSheetBenchWithAlter sheet = (length . toJSONData) (alterCell (CellCord 1 1) "2" (readSheet sheet))


{-
Benchmark spop-calc-bench: RUNNING...
benchmarking readSheet/parseSimpleSheet
time                 9.702 ms   (9.595 ms .. 9.797 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 9.723 ms   (9.651 ms .. 9.808 ms)
std dev              207.0 μs   (164.7 μs .. 280.3 μs)

benchmarking readSheet/parseDeepDepSheet
time                 95.57 ms   (93.97 ms .. 97.24 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 95.03 ms   (94.06 ms .. 95.67 ms)
std dev              1.194 ms   (890.0 μs .. 1.571 ms)

benchmarking readSheet/parseDeepDepSheetAndAlterCell
time                 189.4 ms   (187.3 ms .. 192.8 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 187.6 ms   (186.1 ms .. 188.9 ms)
std dev              1.755 ms   (1.289 ms .. 2.196 ms)
variance introduced by outliers: 14% (moderately inflated)
-}
main :: IO ()
main = defaultMain [
  bgroup "readSheet" [
      bench "parseSimpleSheet" (nf readSheetBench simpleSheet)
      ,bench "parseDeepDepSheet" (nf readSheetBench deepSheet)
      ,bench "parseDeepDepSheetAndAlterCell" (nf readSheetBenchWithAlter deepSheet)
    ]
  ]
