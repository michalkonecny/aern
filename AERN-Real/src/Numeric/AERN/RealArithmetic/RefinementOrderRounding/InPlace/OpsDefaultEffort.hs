{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.OpsDefaultEffort
    Description :  convenience in-place operators and functions with default effort  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience in-place operators and functions with default effort.
-}

module Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.OpsDefaultEffort where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.RealArithmetic.RefinementOrderRounding

-- | Inward rounded in-place addition
addInInPlace :: (RoundedAddInPlace t) => OpMutable2 t s
addInInPlace = mutable2EffToMutable2 addInInPlaceEff addDefaultEffort

-- | Inward rounded addition assignment
(>+<=) :: (RoundedAddInPlace t) => OpMutable1 t s
(>+<=) = mutable2ToMutable1 addInInPlace

-- | Outward rounded in-place addition
addOutInPlace :: (RoundedAddInPlace t) => OpMutable2 t s
addOutInPlace = mutable2EffToMutable2 addOutInPlaceEff addDefaultEffort 

-- | Outward rounded addition assignment
(<+>=) :: (RoundedAddInPlace t) => OpMutable1 t s
(<+>=) = mutable2ToMutable1 addOutInPlace

-- | Inward rounded in-place subtraction
subtrInInPlace :: (RoundedSubtrInPlace t) => OpMutable2 t s
subtrInInPlace = mutable2EffToMutable2 subtrInInPlaceEff addDefaultEffort

-- | Inward rounded subtraction assignment
(>-<=) :: (RoundedSubtrInPlace t) => OpMutable1 t s
(>-<=) = mutable2ToMutable1 subtrInInPlace

-- | Outward rounded in-place subtraction
subtrOutInPlace :: (RoundedSubtrInPlace t) => OpMutable2 t s
subtrOutInPlace = mutable2EffToMutable2 subtrOutInPlaceEff addDefaultEffort

-- | Outward rounded subtraction assignment
(<->=) :: (RoundedSubtrInPlace t) => OpMutable1 t s
(<->=) = mutable2ToMutable1 subtrOutInPlace

-- | Inward rounded in-place absolute value
absInInPlace :: (RoundedAbsInPlace t) => OpMutable1 t s
absInInPlace = mutable1EffToMutable1 absInInPlaceEff absDefaultEffort 

-- | Outward rounded in-place absolute value
absOutInPlace :: (RoundedAbsInPlace t) => OpMutable1 t s
absOutInPlace = mutable1EffToMutable1 absOutInPlaceEff absDefaultEffort 

-- | Inward rounded in-place multiplication
multInInPlace :: (RoundedMultiplyInPlace t) => OpMutable2 t s
multInInPlace = mutable2EffToMutable2 multInInPlaceEff multDefaultEffort

-- | Inward rounded multiplication assignment
(>*<=) :: (RoundedMultiplyInPlace t) => OpMutable1 t s
(>*<=) = mutable2ToMutable1 multInInPlace

-- | Outward rounded in-place multiplication
multOutInPlace :: (RoundedMultiplyInPlace t) => OpMutable2 t s
multOutInPlace = mutable2EffToMutable2 multOutInPlaceEff multDefaultEffort

-- | Outward rounded multiplication assignment
(<*>=) :: (RoundedMultiplyInPlace t) => OpMutable1 t s
(<*>=) = mutable2ToMutable1 multOutInPlace

-- | Inward rounded in-place power
powerToNonnegIntInInPlace :: (RoundedPowerToNonnegIntInPlace t) => 
    OpMutableNonmut t Int s
powerToNonnegIntInInPlace = 
    mutableNonmutEffToMutableNonmut powerToNonnegIntInInPlaceEff powerToNonnegIntDefaultEffort

-- | Inward rounded in-place power assignment
(>^<=) :: (RoundedPowerToNonnegIntInPlace t) => OpNonmut t Int s
(>^<=) = mutableNonmutToNonmut powerToNonnegIntInInPlace

-- | Outward rounded in-place power
powerToNonnegIntOutInPlace :: (RoundedPowerToNonnegIntInPlace t) => 
    OpMutableNonmut t Int s
powerToNonnegIntOutInPlace = 
    mutableNonmutEffToMutableNonmut powerToNonnegIntOutInPlaceEff powerToNonnegIntDefaultEffort

-- | Inward rounded in-place power assignment
(<^>=) :: (RoundedPowerToNonnegIntInPlace t) => OpNonmut t Int s
(<^>=) = mutableNonmutToNonmut powerToNonnegIntOutInPlace

-- | Inward rounded in-place division
divInInPlace :: (RoundedDivideInPlace t) => OpMutable2 t s
divInInPlace = mutable2EffToMutable2 divInInPlaceEff divDefaultEffort

-- | Inward rounded division assignment
(>/<=) :: (RoundedDivideInPlace t) => OpMutable1 t s
(>/<=) = mutable2ToMutable1 divInInPlace

-- | Outward rounded in-place division
divOutInPlace :: (RoundedDivideInPlace t) => OpMutable2 t s
divOutInPlace = mutable2EffToMutable2 divOutInPlaceEff divDefaultEffort

-- | Outward rounded division assignment
(</>=) :: (RoundedDivideInPlace t) => OpMutable1 t s
(</>=) = mutable2ToMutable1 divOutInPlace

-- | Inward rounded in-place mixed addition
mixedAddInInPlace :: (RoundedMixedAddInPlace t tn) => 
    OpMutableNonmut t tn s
mixedAddInInPlace =
    mixedEffToMutableNonmut mixedAddInInPlaceEff mixedAddDefaultEffort

-- | Inward rounded additive scalar action assignment
(>+<|=) :: (RoundedMixedAddInPlace t tn) => OpNonmut t tn s
(>+<|=) = mutableNonmutToNonmut mixedAddInInPlace

-- | Outward rounded in-place mixed addition
mixedAddOutInPlace :: (RoundedMixedAddInPlace t tn) =>
    OpMutableNonmut t tn s
mixedAddOutInPlace =
    mixedEffToMutableNonmut mixedAddOutInPlaceEff mixedAddDefaultEffort

-- | Outward rounded additive scalar action assignment
(<+>|=) :: (RoundedMixedAddInPlace t tn) => OpNonmut t tn s
(<+>|=) = mutableNonmutToNonmut mixedAddOutInPlace

-- | Inward rounded in-place mixed multiplication
mixedMultInInPlace :: (RoundedMixedMultiplyInPlace t tn) => 
    OpMutableNonmut t tn s
mixedMultInInPlace =
    mixedEffToMutableNonmut mixedMultInInPlaceEff mixedMultDefaultEffort

-- | Inward rounded multiplicative scalar action assignment
(>*<|=) :: (RoundedMixedMultiplyInPlace t tn) => OpNonmut t tn s
(>*<|=) = mutableNonmutToNonmut mixedMultInInPlace

-- | Outward rounded in-place mixed multiplication
mixedMultOutInPlace :: (RoundedMixedMultiplyInPlace t tn) => 
    OpMutableNonmut t tn s
mixedMultOutInPlace =
    mixedEffToMutableNonmut mixedMultOutInPlaceEff mixedMultDefaultEffort

-- | Outward rounded multiplicative scalar action assignment
(<*>|=) :: (RoundedMixedMultiplyInPlace t tn) => OpNonmut t tn s
(<*>|=) = mutableNonmutToNonmut mixedMultOutInPlace

-- | Inward rounded in-place mixed reciprocal
mixedDivInInPlace :: (RoundedMixedDivideInPlace t tn) => 
    OpMutableNonmut t tn s
mixedDivInInPlace =
    mixedEffToMutableNonmut mixedDivInInPlaceEff mixedDivDefaultEffort

-- | Inward rounded multiplicative scalar reciprocal action assignment
(>/<|=) :: (RoundedMixedDivideInPlace t tn) => OpNonmut t tn s
(>/<|=) = mutableNonmutToNonmut mixedDivOutInPlace

-- | Outward rounded in-place mixed reciprocal
mixedDivOutInPlace :: (RoundedMixedDivideInPlace t tn) => 
    OpMutableNonmut t tn s
mixedDivOutInPlace =
    mixedEffToMutableNonmut mixedDivOutInPlaceEff mixedDivDefaultEffort

-- | Outward rounded multiplicative scalar reciprocal action assignment
(</>|=) :: (RoundedMixedDivideInPlace t tn) => OpNonmut t tn s
(</>|=) = mutableNonmutToNonmut mixedDivOutInPlace

-- | Inward rounded in-place exponential
expInInPlace :: (RoundedExponentiationInPlace t) => OpMutable1 t s
expInInPlace = mutable1EffToMutable1 expInInPlaceEff expDefaultEffort 

-- | Outward rounded in-place exponential
expOutInPlace :: (RoundedExponentiationInPlace t) => OpMutable1 t s
expOutInPlace = mutable1EffToMutable1 expOutInPlaceEff expDefaultEffort 

-- | Inward rounded in-place square root
sqrtInInPlace :: (RoundedSquareRootInPlace t) => OpMutable1 t s
sqrtInInPlace = mutable1EffToMutable1 sqrtInInPlaceEff sqrtDefaultEffort 

-- | Outward rounded in-place square root
sqrtOutInPlace :: (RoundedSquareRootInPlace t) => OpMutable1 t s
sqrtOutInPlace = mutable1EffToMutable1 sqrtOutInPlaceEff sqrtDefaultEffort 






