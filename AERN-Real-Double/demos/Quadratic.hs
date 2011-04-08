
module Main where

import Numeric.AERN.Interval.Double

main = do
  putStrLn "mi"

-- | 
-- Returns intervals containing the real roots
-- of each real polynomial u*x^2+v*x+w obtained 
-- by choosing coefficients u in a, v in b and w in c. 
quadratic :: DI -> DI -> DI -> [DI]
quadratic a b c 
  | certainlyDoesNotContainZero a =
    quadraticFormula a b c
  | otherwise =
    undefined  

certainlyDoesNotContainZero x =
  case 0 |>=? x of
    Just False -> True
    _ -> False 

quadraticFormula a b c
  | certainlyNonnegative discriminant =
    [(-b-sqrt discriminant)/(2*a),
     (-b+sqrt discriminant)/(2*a)]
  | otherwise =
    undefined
  where
  discriminant = b^2-4*a*c

certainlyNonnegative x =
  case x >=? 0 of
    Just True -> True
    _ -> False 
    

