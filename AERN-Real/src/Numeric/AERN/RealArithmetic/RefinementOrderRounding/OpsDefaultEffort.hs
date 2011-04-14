{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort
    Description :  convenience operators and functions with default effort  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience operators and functions with default effort.
-}

module Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort where

import Numeric.AERN.RealArithmetic.RefinementOrderRounding

infixl 6 <+>, >+<, <->, >-<
infixl 7 <*>, >*<
infixl 8 <^>, >^<
infixl 7 </>, >/<

infixr 6 |<+>, |>+<
infixl 6 <+>|, >+<|
infixr 7 |<*>, |>*<
infixl 7 <*>|, >*<|
infixl 7 </>|, >/<|

-- | Inward rounded addition
(>+<) :: (RoundedAdd t) => t -> t -> t
(>+<) d = addInEff (addDefaultEffort d) d

-- | Outward rounded addition
(<+>) :: (RoundedAdd t) => t -> t -> t
(<+>) d = addOutEff (addDefaultEffort d) d

-- | Inward rounded subtraction
(>-<) :: (RoundedSubtr t) => t -> t -> t
(>-<) d = subtrInEff (addDefaultEffort d) d

-- | Outward rounded subtraction
(<->) :: (RoundedSubtr t) => t -> t -> t
(<->) d = subtrOutEff (addDefaultEffort d) d

-- | Inward rounded absolute value
absIn :: (RoundedAbs t) => t -> t
absIn d = absInEff (absDefaultEffort d) d

-- | Outward rounded absolute value
absOut :: (RoundedAbs t) => t -> t
absOut d = absOutEff (absDefaultEffort d) d

-- | Inward rounded multiplication
(>*<) :: (RoundedMultiply t) => t -> t -> t
(>*<) d = multInEff (multDefaultEffort d) d

-- | Outward rounded multiplication
(<*>) :: (RoundedMultiply t) => t -> t -> t
(<*>) d = multOutEff (multDefaultEffort d) d

-- | Inward rounded power
(>^<) :: (RoundedPowerToNonnegInt t) => t -> Int -> t 
(>^<) d = powerToNonnegIntInEff (powerToNonnegIntDefaultEffort d) d

-- | Outward rounded power
(<^>) :: (RoundedPowerToNonnegInt t) => t -> Int -> t
(<^>) d = powerToNonnegIntOutEff (powerToNonnegIntDefaultEffort d) d

-- | Inward rounded division
(>/<) :: (RoundedDivide t) => t -> t -> t
(>/<) d = divInEff (divDefaultEffort d) d
  
-- | Outward rounded division
(</>) :: (RoundedDivide t) => t -> t -> t
(</>) d = divOutEff (divDefaultEffort d) d

-- | Inward rounded additive scalar left action
(|>+<) :: (RoundedMixedAdd t tn) => tn -> t -> t
(|>+<) n d = mixedAddInEff (mixedAddDefaultEffort d n) d n

-- | Outward rounded additive scalar left action
(|<+>) :: (RoundedMixedAdd t tn) => tn -> t -> t
(|<+>) n d = mixedAddOutEff (mixedAddDefaultEffort d n) d n

-- | Inward rounded additive scalar right action
(>+<|) :: (RoundedMixedAdd t tn) => t -> tn -> t
(>+<|) d n = mixedAddInEff (mixedAddDefaultEffort d n) d n

-- | Outward rounded additive scalar right action
(<+>|) :: (RoundedMixedAdd t tn) => t -> tn -> t
(<+>|) d n = mixedAddOutEff (mixedAddDefaultEffort d n) d n

-- | Inward rounded multiplicative scalar left action
(|>*<) :: (RoundedMixedMultiply t tn) => tn -> t -> t
(|>*<) n d = mixedMultInEff (mixedMultDefaultEffort d n) d n

-- | Outward rounded multiplicative scalar left action
(|<*>) :: (RoundedMixedMultiply t tn) => tn -> t -> t
(|<*>) n d = mixedMultOutEff (mixedMultDefaultEffort d n) d n

-- | Inward rounded multiplicative scalar right action
(>*<|) :: (RoundedMixedMultiply t tn) => t -> tn -> t
(>*<|) d n = mixedMultInEff (mixedMultDefaultEffort d n) d n

-- | Outward rounded multiplicative scalar right action
(<*>|) :: (RoundedMixedMultiply t tn) => t -> tn -> t
(<*>|) d n = mixedMultOutEff (mixedMultDefaultEffort d n) d n

-- | Inward rounded multiplicative scalar reciprocal right action
(>/<|) :: (RoundedMixedDivide t tn) => t -> tn -> t
(>/<|) d n = mixedDivInEff (mixedDivDefaultEffort d n) d n

-- | Outward rounded multiplicative scalar reciprocal right action
(</>|) :: (RoundedMixedDivide t tn) => t -> tn -> t
(</>|) d n = mixedDivOutEff (mixedDivDefaultEffort d n) d n

-- | Inward rounded pi
piIn :: (RoundedSpecialConst t) => t
piIn = result
    where
    result =  
        piInEff (specialConstDefaultEffort result)

-- | Outward rounded pi
piOut :: (RoundedSpecialConst t) => t
piOut = result
    where
    result =  
        piOutEff (specialConstDefaultEffort result)

-- | Inward rounded e
eIn :: (RoundedSpecialConst t) => t
eIn = result
    where
    result =  
        eInEff (specialConstDefaultEffort result)

-- | Outward rounded e
eOut :: (RoundedSpecialConst t) => t
eOut = result
    where
    result =  
        eOutEff (specialConstDefaultEffort result)

-- | Inward rounded exponential
expIn :: (RoundedExponentiation t) => t -> t
expIn d = expInEff (expDefaultEffort d) d

-- | Outward rounded exponential
expOut :: (RoundedExponentiation t) => t -> t
expOut d = expOutEff (expDefaultEffort d) d

-- | Inward rounded square root
sqrtIn :: (RoundedSquareRoot t) => t -> t
sqrtIn d = sqrtInEff (sqrtDefaultEffort d) d

-- | Outward rounded square root
sqrtOut :: (RoundedSquareRoot t) => t -> t
sqrtOut d = sqrtOutEff (sqrtDefaultEffort d) d
