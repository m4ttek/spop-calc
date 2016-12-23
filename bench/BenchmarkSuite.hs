import           Criterion.Main
import           Sheet

{-
benchmarking readSheet/parseSimpleSheet
time                 14.83 ms   (14.64 ms .. 15.06 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 15.14 ms   (15.01 ms .. 15.46 ms)
std dev              527.9 μs   (220.2 μs .. 909.5 μs)
variance introduced by outliers: 11% (moderately inflated)
-}
simpleSheet = [["0" | _ <- [1..100]] | _ <- [1..100]]


{-
benchmarking readSheet/parseDeepDepSheet
time                 244.0 ms   (232.2 ms .. 263.3 ms)
                     0.997 R²   (0.985 R² .. 1.000 R²)
mean                 236.8 ms   (230.2 ms .. 245.9 ms)
std dev              9.698 ms   (4.711 ms .. 14.39 ms)
variance introduced by outliers: 14% (moderately inflated)
-}
-- kolumna CV odpowiada 100
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

main :: IO ()
main = defaultMain [
  bgroup "readSheet" [
      bench "parseSimpleSheet" (nf readSheetBench simpleSheet)
      ,bench "parseDeepDepSheet" (nf readSheetBench deepSheet)
    ]
  ]
