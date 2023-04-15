module Reals where


data Reals = Reals {realsExponent :: Integer, realsCoeffs :: [Int]}
-- Reals e ns represents the real number b^e * (sum_i (ns !! i) b^(-i-1)).
-- the base b is some number (typically 2 or a power of 2), and the entries of
-- ns are allowed to be positive or negative, but bounded by some N. (abs<=N)
-- For example for b=2, N=1, we get sequences of -1,0,+1 to base 2.
-- such a number is bounded by b^e * N/(b-1).
-- As long as 2N/(b-1) is strictly bigger than 1, every real number can be expressed in this way.

-- [0,1,1,...,1] = [1,0,0,....,-1]
-- [-1,1,....,1] = [0,0.....,-1]
-- so we may assume that no two +1 or no two -1 follow one another.

--for now, do b=2, N=2, N'=1

firstDigit :: [Int] -> (Int,[Int])
firstDigit (0:xs) = (0,xs)
firstDigit (2:xs) = (1,xs)
firstDigit (-2:xs) = (-1,xs)
firstDigit (1:(-2):xs) = (0,0:xs)
firstDigit (1:(-1):xs) = (0,1:xs)
firstDigit (1:0:xs) = (0,2:xs)
firstDigit (1:1:xs) = (1,(-1):xs)
firstDigit (1:2:xs) = (1,0:xs)
firstDigit ((-1):2:xs) = (0,0:xs)
firstDigit ((-1):1:xs) = (0,(-1):xs)
firstDigit ((-1):0:xs) = (0,(-2):xs)
firstDigit ((-1):(-1):xs) = ((-1),1:xs)
firstDigit ((-1):(-2):xs) = ((-1),2:xs)

--recomputes the exponent by dropping up to maxDrop many 0's
--(we don't want to do this all the way, since that might run very long if applied to an extremely small number)
recomputeExponent :: Integer -> Reals -> Reals
recomputeExponent maxDrop r | maxDrop <= 0 = r
recomputeExponent maxDrop (Reals e (0:ns))  = recomputeExponent (maxDrop -1) (Reals (e-1) ns)
recomputeExponent maxDrop r = r

reduce :: Reals -> Reals
reduce (Reals e ns) = recomputeExponent 1 (Reals (e+1) ns') where
  ns' = unfold firstDigit ns
  unfold f y = let (x,y') = f y in x : unfold f y'

--a bit hacky, just fix precision.
--In an ideal world we would use floatDigits etc to inspect the implementation of a.

toFloat :: (RealFloat a) => Reals -> a
toFloat (Reals e ns) = sum [fromIntegral c*2**(fromIntegral $ e-i) | (i,c) <- zip [1..30] ns]

add :: Reals -> Reals -> Reals
add (Reals e1 ns1) (Reals e2 ns2) | e1>=e2 = reduce $ Reals e1 
                                     ((take (fromIntegral $ e1-e2) ns1)
                                    ++ zipWith (+) (drop (fromIntegral $ e1-e2) ns1) ns2)
add r1 r2 = add r2 r1

digits :: Integer -> [Int]
digits 0 = []
digits n = let (n',e) = n`divMod`2
            in fromIntegral e:(digits n')

intToReals :: Integer -> Reals 
intToReals n = let e = ceiling (log (fromIntegral n+0.5) / log 2)
               in Reals e (reverse (digits n) ++ repeat 0)

truncateorpad :: Integer -> Reals -> [Int]
truncateorpad e (Reals e0 ns) | e0>e = drop (fromIntegral (e0-e)) ns
                              |otherwise = (replicate (fromIntegral (e-e0)) 0) ++ ns

assertExponent :: Integer -> Reals -> Reals
assertExponent e r = Reals e (truncateorpad e r)

instance Show Reals where
 show r = show (toFloat r :: Double)

instance Num Reals where
 fromInteger = intToReals
 (+) = add
 negate (Reals e ns) = Reals e (map negate ns)



-- with current strictness, some fixed point iterations work lazily! For example:
x :: Reals
x = assertExponent 1 $ 1 + half (half (half x)) where
 half (Reals e ns) = Reals (e-1) ns

--todo: implement multiplication and division
--todo: implement exponential function and various other analytic functions
--todo: implement lazy streaming version of Newton iteration
