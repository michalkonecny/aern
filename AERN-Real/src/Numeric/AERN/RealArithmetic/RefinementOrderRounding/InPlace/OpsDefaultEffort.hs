{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.OpsDefaultEffort
    Description :  convenience in-place operators and functions with default effort  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience in-place operators and functions with default effort.
-}

module Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.OpsDefaultEffort where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.RealArithmetic.RefinementOrderRounding
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace

-- | Inward rounded in place addition
addInInPlace :: (RoundedAddInPlace t) => t -> OpMutable2 t s
addInInPlace d = addInInPlaceEff d (addDefaultEffort d) 

-- | Outward rounded in place addition
addOutInPlace :: (RoundedAddInPlace t) => t -> OpMutable2 t s
addOutInPlace d = addOutInPlaceEff d (addDefaultEffort d) 

-- | Inward rounded in place subtraction
subtrInInPlace :: (RoundedSubtrInPlace t) => t -> OpMutable2 t s
subtrInInPlace d = subtrInInPlaceEff d (addDefaultEffort d)

-- | Outward rounded in place subtraction
subtrOutInPlace :: (RoundedSubtrInPlace t) => t -> OpMutable2 t s
subtrOutInPlace d = subtrOutInPlaceEff d (addDefaultEffort d)

-- | Inward rounded in place absolute value
absInInPlace :: (RoundedAbsInPlace t) => t -> OpMutable1 t s
absInInPlace d = absInInPlaceEff d (absDefaultEffort d) 

-- | Outward rounded in place absolute value
absOutInPlace :: (RoundedAbsInPlace t) => t -> OpMutable1 t s
absOutInPlace d = absOutInPlaceEff d (absDefaultEffort d) 

-- | Inward rounded in place multiplication
multInInPlace :: (RoundedMultiplyInPlace t) => t -> OpMutable2 t s
multInInPlace d = multInInPlaceEff d (multDefaultEffort d)

-- | Outward rounded in place multiplication
multOutInPlace :: (RoundedMultiplyInPlace t) => t -> OpMutable2 t s
multOutInPlace d = multOutInPlaceEff d (multDefaultEffort d)

-- | Inward rounded in place power
powerToNonnegIntInInPlace :: (RoundedPowerToNonnegIntInPlace t) => t -> OpMutableNonmut t Int s
powerToNonnegIntInInPlace d = powerToNonnegIntInInPlaceEff d (powerToNonnegIntDefaultEffort d)

-- | Outward rounded in place power
powerToNonnegIntOutInPlace :: (RoundedPowerToNonnegIntInPlace t) => t -> OpMutableNonmut t Int s
powerToNonnegIntOutInPlace d = powerToNonnegIntOutInPlaceEff d (powerToNonnegIntDefaultEffort d)

-- | Inward rounded in place division
divInInPlace :: (RoundedDivideInPlace t) => t -> OpMutable2 t s
divInInPlace d = divInInPlaceEff d (divDefaultEffort d)

-- | Outward rounded in place division
divOutInPlace :: (RoundedDivideInPlace t) => t -> OpMutable2 t s
divOutInPlace d = divOutInPlaceEff d (divDefaultEffort d)

-- | Inward rounded in place additive scalar left action

-- | Outward rounded in place additive scalar left action

-- | Inward rounded in place additive scalar right action

-- | Outward rounded in place additive scalar right action

-- | Inward rounded in place multiplicative scalar left action

-- | Outward rounded in place multiplicative scalar left action

-- | Inward rounded in place multiplicative scalar right action

-- | Outward rounded in place multiplicative scalar right action

-- | Inward rounded in place multiplicative scalar reciprocal right action

-- | Outward rounded in place multiplicative scalar reciprocal right action


