
module Main where 

import Numeric.AERN.DoubleBasis.Interval

main =
  do
  putStrLn $ "zero   0.1 (\\x -> x^2-0.5) (Interval 1 2) = " ++ 
    show (zero 0.1 (\x -> x^2-0.5) (Interval 1 2))  
  putStrLn $ "zero   0.1 (\\x -> x^2-0.5) (Interval 0 1) = " ++ 
    show (zero 0.1 (\x -> x^2-0.5) (Interval 0 1))
  putStrLn $ "zero  0.01 (\\x -> x^2-0.5) (Interval 0 1) = " ++ 
    show (zero 0.01 (\x -> x^2-0.5) (Interval 0 1))
  putStrLn $ "zero 0.001 (\\x -> x^2-0.5) (Interval 0 1) = " ++ 
    show (zero 0.001 (\x -> x^2-0.5) (Interval 0 1))

-- find leftmost zero of f in d to within e accuracy
zero :: DI -> (DI -> DI) -> DI -> Maybe DI
zero e f d =
  zero' e f [d]

zero' _ _ [] = Nothing 
zero' e f (d:ds)
  | imageContainsZero == Just False = 
      zero' e f ds
  | imageContainsZero == Just True && reachedSufficientAccuracy = 
      Just d
  | cannotSplit =
      error $ "zero: cannot split interval " ++ show d
  | otherwise = 
      zero' e f (dl:dr:ds)
  where
  cannotSplit = (dl |==? d) == Just True || (dr |==? d) == Just True
  imageContainsZero = fd |<=? 0 
  reachedSufficientAccuracy = 
    case width fd <? e of
      Just True -> True
      _ -> False
  (dl,dr) = bisect d
  fd = f d

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
