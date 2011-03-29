
module Riemann where

import Numeric.AERN.Basics.NumericOrder.OpsDefaultEffort
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.RefinementOrder.OpsDefaultEffort
import Numeric.AERN.RealArithmetic.Basis.Double
import Numeric.AERN.RealArithmetic.Interval
import Numeric.AERN.RealArithmetic.Interval.Double
import Numeric.AERN.RealArithmetic.RefinementOrderRounding
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort
import Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsDefaultEffort
import Numeric.AERN.RealArithmetic.Measures
import Numeric.AERN.Basics.Interval

-- compute the integral of f over d to within e accuracy
riemann :: DI -> (DI -> DI) -> DI -> DI
riemann e f d =
  riemann' e f initWidthI [d] 0
  where
  initWidthI = Interval initWidth initWidth
  initWidth = r - l
  Interval l r = d


riemann' _ _ _ []     result = result 
riemann' e f initWidth (d:ds) result
  | reachedSufficientAccuracy =
      riemann' e f initWidth ds (dArea - 0.5*dError + result)
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
  dr = Interval midpoint r
  dl = Interval l midpoint
  midpoint = 0.5*(l + r)
  Interval l r = d

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
  imageContainsZero = fd |<=? 0 
  reachedSufficientAccuracy = 
    case width fd <? e of
      Just True -> True
      _ -> False
  cannotSplit = l == midpoint || midpoint == r
  fd = f d
  dr = Interval midpoint r
  dl = Interval l midpoint
  midpoint = 0.5*(l + r)
  Interval l r = d  

width i = imprecisionOfEff (imprecisionDefaultEffort i) i




