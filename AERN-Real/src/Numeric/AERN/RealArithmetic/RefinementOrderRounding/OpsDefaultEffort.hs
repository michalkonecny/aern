{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
    Description :  convenience binary infix operators with default effort  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience binary infix operators with default effort.
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

-- | Inner rounded addition
(>+<) :: (RoundedAdd t) => t -> t -> t
(>+<) d = addInEff (addDefaultEffort d) d

-- | Outer rounded addition
(<+>) :: (RoundedAdd t) => t -> t -> t
(<+>) d = addOutEff (addDefaultEffort d) d

-- | Inner rounded subtraction
(>-<) :: (RoundedSubtr t) => t -> t -> t
(>-<) d = subtrInEff (addDefaultEffort d) d

-- | Outer rounded subtraction
(<->) :: (RoundedSubtr t) => t -> t -> t
(<->) d = subtrOutEff (addDefaultEffort d) d

-- | Inner rounded absolute value
absIn :: (RoundedAbs t) => t -> t
absIn d = absInEff (absDefaultEffort d) d

-- | Outer rounded absolute value
absOut :: (RoundedAbs t) => t -> t
absOut d = absOutEff (absDefaultEffort d) d

-- | Inner rounded multiplication
(>*<) :: (RoundedMultiply t) => t -> t -> t
(>*<) d = multInEff (multDefaultEffort d) d

-- | Outer rounded multiplication
(<*>) :: (RoundedMultiply t) => t -> t -> t
(<*>) d = multOutEff (multDefaultEffort d) d

-- | Inner rounded power
(>^<) :: (RoundedPowerToNonnegInt t) => t -> Int -> t 
(>^<) d = powerToNonnegIntInEff (powerToNonnegIntDefaultEffort d) d

-- | Outer rounded power
(<^>) :: (RoundedPowerToNonnegInt t) => t -> Int -> t
(<^>) d = powerToNonnegIntOutEff (powerToNonnegIntDefaultEffort d) d

-- | Inner rounded division
(>/<) :: (RoundedDivide t) => t -> t -> t
(>/<) d = divInEff (divDefaultEffort d) d

-- | Outer rounded division
(</>) :: (RoundedDivide t) => t -> t -> t
(</>) d = divOutEff (divDefaultEffort d) d

-- | Inner rounded additive scalar left action
(|>+<) :: (RoundedMixedAdd t tn) => tn -> t -> t
(|>+<) n d = mixedAddInEff (mixedAddDefaultEffort d n) d n

-- | Outer rounded additive scalar left action
(|<+>) :: (RoundedMixedAdd t tn) => tn -> t -> t
(|<+>) n d = mixedAddOutEff (mixedAddDefaultEffort d n) d n

-- | Inner rounded additive scalar right action
(>+<|) :: (RoundedMixedAdd t tn) => t -> tn -> t
(>+<|) d n = mixedAddInEff (mixedAddDefaultEffort d n) d n

-- | Outer rounded additive scalar right action
(<+>|) :: (RoundedMixedAdd t tn) => t -> tn -> t
(<+>|) d n = mixedAddOutEff (mixedAddDefaultEffort d n) d n

-- | Inner rounded multiplicative scalar left action
(|>*<) :: (RoundedMixedMultiply t tn) => tn -> t -> t
(|>*<) n d = mixedMultInEff (mixedMultDefaultEffort d n) d n

-- | Outer rounded multiplicative scalar left action
(|<*>) :: (RoundedMixedMultiply t tn) => tn -> t -> t
(|<*>) n d = mixedMultOutEff (mixedMultDefaultEffort d n) d n

-- | Inner rounded multiplicative scalar right action
(>*<|) :: (RoundedMixedMultiply t tn) => t -> tn -> t
(>*<|) d n = mixedMultInEff (mixedMultDefaultEffort d n) d n

-- | Outer rounded multiplicative scalar right action
(<*>|) :: (RoundedMixedMultiply t tn) => t -> tn -> t
(<*>|) d n = mixedMultOutEff (mixedMultDefaultEffort d n) d n

-- | Inner rounded multiplicative scalar reciprocal right action
(>/<|) :: (RoundedMixedDivide t tn) => t -> tn -> t
(>/<|) d n = mixedDivInEff (mixedDivDefaultEffort d n) d n

-- | Outer rounded multiplicative scalar reciprocal right action
(</>|) :: (RoundedMixedDivide t tn) => t -> tn -> t
(</>|) d n = mixedDivOutEff (mixedDivDefaultEffort d n) d n

-- | Inner rounded pi
piIn :: (RoundedSpecialConst t) => t
piIn = result
    where
    result =  
        piInEff (specialConstDefaultEffort result)

-- | Outer rounded pi
piOut :: (RoundedSpecialConst t) => t
piOut = result
    where
    result =  
        piOutEff (specialConstDefaultEffort result)

-- | Inner rounded e
eIn :: (RoundedSpecialConst t) => t
eIn = result
    where
    result =  
        eInEff (specialConstDefaultEffort result)

-- | Outer rounded e
eOut :: (RoundedSpecialConst t) => t
eOut = result
    where
    result =  
        eOutEff (specialConstDefaultEffort result)

-- | Inner rounded exponential
expIn :: (RoundedExponentiation t) => t -> t
expIn d = expInEff (expDefaultEffort d) d

-- | Outer rounded exponential
expOut :: (RoundedExponentiation t) => t -> t
expOut d = expOutEff (expDefaultEffort d) d

-- | Inner rounded square root
sqrtIn :: (RoundedSquareRoot t) => t -> t
sqrtIn d = sqrtInEff (sqrtDefaultEffort d) d

-- | Outer rounded square root
sqrtOut :: (RoundedSquareRoot t) => t -> t
sqrtOut d = sqrtOutEff (sqrtDefaultEffort d) d
