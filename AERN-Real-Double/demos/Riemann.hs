
module Riemann where

import Numeric.AERN.RealArithmetic.Basis.Double
import Numeric.AERN.RealArithmetic.Interval.Double

main =
  do
  putStrLn $ "riemann 0.1 (\\x -> x^2) (Interval 0 1) = " ++
    show (riemann 0.1 (\x -> x^2) (Interval 0 1))
  putStrLn $ "erf 0.001 1 = " ++ show (erf 0.001 1) 

-- compute the error function to within e accuracy
erf :: DI -> DI -> DI
erf e x =
  2/(sqrtOut (piOut 0)) * riemann e (\t -> expOut (-t^2)) (0 </\> x)

-- compute the integral of f over d to within e accuracy
riemann :: DI -> (DI -> DI) -> DI -> DI
riemann e f d =
  riemann' e f (width d) [d] 0

riemann' _ _ _ []     result = result 
riemann' e f initWidth (d:ds) result
  | reachedSufficientAccuracy =
      riemann' e f initWidth ds (dArea + result)
  | otherwise = 
      riemann' e f initWidth (dl:dr:ds) result
  where
  reachedSufficientAccuracy =
    case dError <? e * dWidth / initWidth of
      Just True -> True
      _ -> False
  dError = dWidth * (width fd)
  dArea = dWidth * fd
  fd = f d
  dWidth = width d
  (dl, dr) = bisect d

width i = 
  irI <-> ilI
  where
  irI = Interval ir ir  
  ilI = Interval il il
  Interval il ir = i  

bisect i =
  (l,r)
  where
  r = Interval midpoint ir
  l = Interval il midpoint
  midpoint = 0.5*(il + ir)
  Interval il ir = i
