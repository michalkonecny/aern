
module Main where

import Numeric.AERN.DoubleBasis.Interval
import Data.List


main = do
  putStrLn $ "quadratic 1 1 1 = " ++ show (quadratic 1 1 1)
  putStrLn $ "quadratic 1 2 1 = " ++ show (quadratic 1 2 1)
  putStrLn $ "quadratic 1 5 6 = " ++ show (quadratic 1 5 6)

-- | 
-- Returns list such that the union of its members contains all
-- real roots of each real polynomial u*x^2+v*x+w obtained by
-- choosing coefficients u in a, v in b and w in c. 
quadratic :: DI -> DI -> DI -> [DI]
quadratic a b c 
  | certainlyDoesNotContainZero a =
    quadraticFormula a b c
  | otherwise =
    [bottom]

certainlyDoesNotContainZero x =
  case 0 |>=? x of
    Just False -> True
    _ -> False 

quadraticFormula a b c
  | certainlyZero discriminant =
    [-b/(2*a)]
  | certainlyNonnegative discriminant =
    [(-b-sqrt discriminant)/(2*a),
     (-b+sqrt discriminant)/(2*a)]
  | certainlyNegative discriminant =
    []
  | otherwise =
    [bottom]
  where
  discriminant = b^2-4*a*c

certainlyZero x =
  case x ==? 0 of
    Just True -> True
    _ -> False 

certainlyNonnegative x =
  case x >=? 0 of
    Just True -> True
    _ -> False 

certainlyNegative x =
  case x <? 0 of
    Just True -> True
    _ -> False 
