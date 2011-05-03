
module Main where

import Numeric.AERN.DoubleBasis.Interval
import Numeric.AERN.DoubleBasis.MInterval

import Control.Monad.ST (ST, runST)

main =
  do
  putStrLn $ "riemann 0.1 (\\x -> x^2) (Interval 0 1) = " ++
    show (riemann 0.1 (\x -> x^2) (Interval 0 1))
  putStrLn "erf e x = 2/(sqrt pi) * riemann e (\\t -> exp (-t^2)) (0 </\\> x)"
  putStrLn $ "erf 0.001 1 = " ++ show (erf 0.001 1) 
  putStrLn $ "riemannInPlace 0.1 (\\x -> x^2) (Interval 0 1) = " ++
    show (riemannInPlace 0.1 (\x -> x^2) (Interval 0 1))
  putStrLn "erfInPlace e x = 2/(sqrt pi) * riemannInPlace e (\\t -> exp (-t^2)) (0 </\\> x)"
  putStrLn $ "erfInPlace 0.001 1 = " ++ show (erfInPlace 0.001 1) 
  putStrLn $ "riemannInPlace 0.1 (\\x -> x^2) (Interval 0 1) = " ++
    show (riemannInPlace 0.1 (\x -> x^2) (Interval 0 1))
  putStrLn "erfInPlace e x = 2/(sqrt pi) * riemannInPlace e (\\t -> exp (-t^2)) (0 </\\> x)"
  putStrLn $ "erfInPlace 0.001 1 = " ++ show (erf 0.001 1) 

-- compute the error function to within e accuracy
erf :: DI -> DI -> DI
erf e x =
  2/(sqrt pi) * riemann e (\t -> exp (-t^2)) (0 </\> x)

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

-- erf using riemannInPlace
erfInPlace :: DI -> DI -> DI
erfInPlace e x =
  2/(sqrt pi) * riemannInPlace e (\t -> exp (-t^2)) (0 </\> x)

-- riemann using in-place accumulator
riemannInPlace :: DI -> (DI -> DI) -> DI -> DI
riemannInPlace e f d =
  runST $
    do
    resM <- makeMutable 0
    riemannInPlace' resM e f (width d) [d]
    result <- unsafeReadMutable resM
    return result

riemannInPlace' resM _ _ _ [] =
  return () 
riemannInPlace' resM e f initWidth (d:ds)
  | reachedSufficientAccuracy =
      do 
      dAreaM <- unsafeMakeMutable dArea
      resM <+>= dAreaM
      riemannInPlace' resM e f initWidth ds 
  | otherwise = 
      riemannInPlace' resM e f initWidth (dl:dr:ds)
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
