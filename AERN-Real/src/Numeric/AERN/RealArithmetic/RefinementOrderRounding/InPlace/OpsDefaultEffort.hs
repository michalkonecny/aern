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

-- | Inward rounded in place addition
addInInPlace :: (CanBeMutable t, RoundedAdd t) => OpMutable2 t s
addInInPlace = pureEffToMutable2 addInEff addDefaultEffort

-- associated assignment operator

-- | Inward rounded addition assignment
(>+<=) :: (CanBeMutable t, RoundedAdd t) => OpMutable1 t s
(>+<=) = mutable2ToMutable1 addInInPlace

-- | Outward rounded in place addition
addOutInPlace :: (CanBeMutable t, RoundedAdd t) => OpMutable2 t s
addOutInPlace = pureEffToMutable2 addOutEff addDefaultEffort 

-- | Outward rounded addition assignment
(<+>=) :: (CanBeMutable t, RoundedAdd t) => OpMutable1 t s
(<+>=) = mutable2ToMutable1 addOutInPlace

-- | Inward rounded in place subtraction
subtrInInPlace :: (CanBeMutable t, RoundedSubtr t) => OpMutable2 t s
subtrInInPlace = pureEffToMutable2 subtrInEff addDefaultEffort

-- | Inward rounded subtraction assignment
(>-<=) :: (CanBeMutable t, RoundedSubtr t) => OpMutable1 t s
(>-<=) = mutable2ToMutable1 subtrInInPlace

-- | Outward rounded in place subtraction
subtrOutInPlace :: (CanBeMutable t, RoundedSubtr t) => OpMutable2 t s
subtrOutInPlace = pureEffToMutable2 subtrOutEff addDefaultEffort

-- | Outward rounded subtraction assignment
(<->=) :: (CanBeMutable t, RoundedSubtr t) => OpMutable1 t s
(<->=) = mutable2ToMutable1 subtrOutInPlace

-- | Inward rounded in place absolute value
absInInPlace :: (CanBeMutable t, RoundedAbs t) => OpMutable1 t s
absInInPlace = pureEffToMutable1 absInEff absDefaultEffort 

-- | Outward rounded in place absolute value
absOutInPlace :: (CanBeMutable t, RoundedAbs t) => OpMutable1 t s
absOutInPlace = pureEffToMutable1 absOutEff absDefaultEffort 

-- | Inward rounded in place multiplication
multInInPlace :: (CanBeMutable t, RoundedMultiply t) => OpMutable2 t s
multInInPlace = pureEffToMutable2 multInEff multDefaultEffort

-- | Inward rounded multiplication assignment
(>*<=) :: (CanBeMutable t, RoundedMultiply t) => OpMutable1 t s
(>*<=) = mutable2ToMutable1 multInInPlace

-- | Outward rounded in place multiplication
multOutInPlace :: (CanBeMutable t, RoundedMultiply t) => OpMutable2 t s
multOutInPlace = pureEffToMutable2 multOutEff multDefaultEffort

-- | Outward rounded multiplication assignment
(<*>=) :: (CanBeMutable t, RoundedMultiply t) => OpMutable1 t s
(<*>=) = mutable2ToMutable1 multOutInPlace

-- | Inward rounded in place power
powerToNonnegIntInInPlace :: (CanBeMutable t, RoundedPowerToNonnegInt t) => 
    OpMutableNonmut t Int s
powerToNonnegIntInInPlace = 
    pureEffToMutableNonmut powerToNonnegIntInEff powerToNonnegIntDefaultEffort

-- | Inward rounded in place power assignment
(>^<=) :: (CanBeMutable t, RoundedPowerToNonnegInt t) => OpNonmut t Int s
(>^<=) = mutableNonmutToNonmut powerToNonnegIntInInPlace

-- | Outward rounded in place power
powerToNonnegIntOutInPlace :: (CanBeMutable t, RoundedPowerToNonnegInt t) => 
    OpMutableNonmut t Int s
powerToNonnegIntOutInPlace = 
    pureEffToMutableNonmut powerToNonnegIntOutEff powerToNonnegIntDefaultEffort

-- | Inward rounded in place power assignment
(<^>=) :: (CanBeMutable t, RoundedPowerToNonnegInt t) => OpNonmut t Int s
(<^>=) = mutableNonmutToNonmut powerToNonnegIntOutInPlace

-- | Inward rounded in place division
divInInPlace :: (CanBeMutable t, RoundedDivide t) => OpMutable2 t s
divInInPlace = pureEffToMutable2 divInEff divDefaultEffort

-- | Inward rounded division assignment
(>/<=) :: (CanBeMutable t, RoundedDivide t) => OpMutable1 t s
(>/<=) = mutable2ToMutable1 divInInPlace

-- | Outward rounded in place division
divOutInPlace :: (CanBeMutable t, RoundedDivide t) => OpMutable2 t s
divOutInPlace = pureEffToMutable2 divOutEff divDefaultEffort

-- | Outward rounded division assignment
(</>=) :: (CanBeMutable t, RoundedDivide t) => OpMutable1 t s
(</>=) = mutable2ToMutable1 divOutInPlace

-- | Inward rounded in place mixed addition
mixedAddInInPlace :: (CanBeMutable t, RoundedMixedAdd t tn) => 
    OpMutableNonmut t tn s
mixedAddInInPlace =
    pureMixedEffToMutableNonmut mixedAddInEff mixedAddDefaultEffort

-- | Inward rounded additive scalar action assignment
(>+<|=) :: (CanBeMutable t, RoundedMixedAdd t tn) => OpNonmut t tn s
(>+<|=) = mutableNonmutToNonmut mixedAddInInPlace

-- | Outward rounded in place mixed addition
mixedAddOutInPlace :: (CanBeMutable t, RoundedMixedAdd t tn) =>
    OpMutableNonmut t tn s
mixedAddOutInPlace =
    pureMixedEffToMutableNonmut mixedAddOutEff mixedAddDefaultEffort

-- | Outward rounded additive scalar action assignment
(<+>|=) :: (CanBeMutable t, RoundedMixedAdd t tn) => OpNonmut t tn s
(<+>|=) = mutableNonmutToNonmut mixedAddOutInPlace

-- | Inward rounded in place mixed multiplication
mixedMultInInPlace :: (CanBeMutable t, RoundedMixedMultiply t tn) => 
    OpMutableNonmut t tn s
mixedMultInInPlace =
    pureMixedEffToMutableNonmut mixedMultInEff mixedMultDefaultEffort

-- | Inward rounded multiplicative scalar action assignment
(>*<|=) :: (CanBeMutable t, RoundedMixedMultiply t tn) => OpNonmut t tn s
(>*<|=) = mutableNonmutToNonmut mixedMultInInPlace

-- | Outward rounded in place mixed multiplication
mixedMultOutInPlace :: (CanBeMutable t, RoundedMixedMultiply t tn) => 
    OpMutableNonmut t tn s
mixedMultOutInPlace =
    pureMixedEffToMutableNonmut mixedMultOutEff mixedMultDefaultEffort

-- | Outward rounded multiplicative scalar action assignment
(<*>|=) :: (CanBeMutable t, RoundedMixedMultiply t tn) => OpNonmut t tn s
(<*>|=) = mutableNonmutToNonmut mixedMultOutInPlace

-- | Inward rounded in place mixed reciprocal
mixedDivInInPlace :: (CanBeMutable t, RoundedMixedDivide t tn) => 
    OpMutableNonmut t tn s
mixedDivInInPlace =
    pureMixedEffToMutableNonmut mixedDivInEff mixedDivDefaultEffort

-- | Inward rounded multiplicative scalar reciprocal action assignment
(>/<|=) :: (CanBeMutable t, RoundedMixedDivide t tn) => OpNonmut t tn s
(>/<|=) = mutableNonmutToNonmut mixedDivOutInPlace

-- | Outward rounded in place mixed reciprocal
mixedDivOutInPlace :: (CanBeMutable t, RoundedMixedDivide t tn) => 
    OpMutableNonmut t tn s
mixedDivOutInPlace =
    pureMixedEffToMutableNonmut mixedDivOutEff mixedDivDefaultEffort

-- | Outward rounded multiplicative scalar reciprocal action assignment
(</>|=) :: (CanBeMutable t, RoundedMixedDivide t tn) => OpNonmut t tn s
(</>|=) = mutableNonmutToNonmut mixedDivOutInPlace
