--Źródło: http://stackoverflow.com/questions/30931369/how-to-convert-a-rational-into-a-pretty-string

module ShowRational where
import           Data.List (findIndex, splitAt)

-- | Convert a 'Rational' to a 'String' using the given number of decimals.
-- If the number of decimals is not given the full precision is showed using (DDD) for repeating digits.
-- E.g., 13.7/3 is shown as \"4.5(6)\".
showRational :: Maybe Int -> Rational -> String
showRational (Just n) r =
    let d = round (abs r * 10^n)
        s = show (d :: Integer)
        s' = replicate (n - length s + 1) '0' ++ s
        (h, f) = splitAt (length s' - n) s'
    in  (if r < 0 then "-" else "") ++ h ++ "." ++ f
-- The length of the repeating digits is related to the totient function of the denominator.
-- This means that the complexity of computing them is at least as bad as factoring, i.e., it quickly becomes infeasible.
showRational Nothing r =
    let (i, f) = properFraction (abs r) :: (Integer, Rational)
        si = if r < 0 then "-" ++ show i else show i
        decimals f = loop f [] ""
        loop x fs ds =
            if x == 0 then
                ds
            else
                case findIndex (x ==) fs of
                    Just i  -> let (l, r) = splitAt i ds in l ++ "(" ++ r ++ ")"
                    Nothing -> let (c, f) = properFraction (10 * x) :: (Integer, Rational) in loop f (fs ++ [x]) (ds ++ show c)
    in  if f == 0 then si else si ++ "." ++ decimals f
