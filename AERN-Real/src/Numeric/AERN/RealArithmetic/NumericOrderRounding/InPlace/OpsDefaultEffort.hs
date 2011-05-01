{-|
    Module      :  Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.OpsDefaultEffort
    Description :  convenience in-place operators and functions with default effort  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience in-place operators and functions with default effort.
-}

module Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.OpsDefaultEffort where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.RealArithmetic.NumericOrderRounding

-- | Upward rounded in-place addition
addUpInPlace :: (RoundedAddInPlace t) => OpMutable2 t s
addUpInPlace = mutable2EffToMutable2 addUpInPlaceEff addDefaultEffort

-- | Upward rounded addition assignment
(+^=) :: (RoundedAddInPlace t) => OpMutable1 t s
(+^=) = mutable2ToMutable1 addUpInPlace

-- | Downward rounded in-place addition
addDnInPlace :: (RoundedAddInPlace t) => OpMutable2 t s
addDnInPlace = mutable2EffToMutable2 addDnInPlaceEff addDefaultEffort 

-- | Downward rounded addition assignment
(+.=) :: (RoundedAddInPlace t) => OpMutable1 t s
(+.=) = mutable2ToMutable1 addDnInPlace

-- | Upward rounded in-place subtraction
subtrUpInPlace :: (RoundedSubtrInPlace t) => OpMutable2 t s
subtrUpInPlace = mutable2EffToMutable2 subtrUpInPlaceEff addDefaultEffort

-- | Upward rounded subtraction assignment
(-^=) :: (RoundedSubtrInPlace t) => OpMutable1 t s
(-^=) = mutable2ToMutable1 subtrUpInPlace

-- | Downward rounded in-place subtraction
subtrDnInPlace :: (RoundedSubtrInPlace t) => OpMutable2 t s
subtrDnInPlace = mutable2EffToMutable2 subtrDnInPlaceEff addDefaultEffort

-- | Downward rounded subtraction assignment
(-.=) :: (RoundedSubtrInPlace t) => OpMutable1 t s
(-.=) = mutable2ToMutable1 subtrDnInPlace

-- | Upward rounded in-place absolute value
absUpInPlace :: (RoundedAbsInPlace t) => OpMutable1 t s
absUpInPlace = mutable1EffToMutable1 absUpInPlaceEff absDefaultEffort 

-- | Downward rounded in-place absolute value
absDnInPlace :: (RoundedAbsInPlace t) => OpMutable1 t s
absDnInPlace = mutable1EffToMutable1 absDnInPlaceEff absDefaultEffort 

-- | Upward rounded in-place multiplication
multUpInPlace :: (RoundedMultiplyInPlace t) => OpMutable2 t s
multUpInPlace = mutable2EffToMutable2 multUpInPlaceEff multDefaultEffort

-- | Upward rounded multiplication assignment
(*^=) :: (RoundedMultiplyInPlace t) => OpMutable1 t s
(*^=) = mutable2ToMutable1 multUpInPlace

-- | Downward rounded in-place multiplication
multDnInPlace :: (RoundedMultiplyInPlace t) => OpMutable2 t s
multDnInPlace = mutable2EffToMutable2 multDnInPlaceEff multDefaultEffort

-- | Downward rounded multiplication assignment
(*.=) :: (RoundedMultiplyInPlace t) => OpMutable1 t s
(*.=) = mutable2ToMutable1 multDnInPlace

-- | Upward rounded in-place power
powerToNonnegIntUpInPlace :: (RoundedPowerToNonnegIntInPlace t) => 
    OpMutableNonmut t Int s
powerToNonnegIntUpInPlace = 
    mutableNonmutEffToMutableNonmut powerToNonnegIntUpInPlaceEff powerToNonnegIntDefaultEffort

-- | Upward rounded in-place power assignment
(^^=) :: (RoundedPowerToNonnegIntInPlace t) => OpNonmut t Int s
(^^=) = mutableNonmutToNonmut powerToNonnegIntUpInPlace

-- | Downward rounded in-place power
powerToNonnegIntDnInPlace :: (RoundedPowerToNonnegIntInPlace t) => 
    OpMutableNonmut t Int s
powerToNonnegIntDnInPlace = 
    mutableNonmutEffToMutableNonmut powerToNonnegIntDnInPlaceEff powerToNonnegIntDefaultEffort

-- | Upward rounded in-place power assignment
(^.=) :: (RoundedPowerToNonnegIntInPlace t) => OpNonmut t Int s
(^.=) = mutableNonmutToNonmut powerToNonnegIntDnInPlace

-- | Upward rounded in-place division
divUpInPlace :: (RoundedDivideInPlace t) => OpMutable2 t s
divUpInPlace = mutable2EffToMutable2 divUpInPlaceEff divDefaultEffort

-- | Upward rounded division assignment
(/^=) :: (RoundedDivideInPlace t) => OpMutable1 t s
(/^=) = mutable2ToMutable1 divUpInPlace

-- | Downward rounded in-place division
divDnInPlace :: (RoundedDivideInPlace t) => OpMutable2 t s
divDnInPlace = mutable2EffToMutable2 divDnInPlaceEff divDefaultEffort

-- | Downward rounded division assignment
(/.=) :: (RoundedDivideInPlace t) => OpMutable1 t s
(/.=) = mutable2ToMutable1 divDnInPlace

-- | Upward rounded in-place mixed addition
mixedAddUpInPlace :: (RoundedMixedAddInPlace t tn) => 
    OpMutableNonmut t tn s
mixedAddUpInPlace =
    mixedEffToMutableNonmut mixedAddUpInPlaceEff mixedAddDefaultEffort

-- | Upward rounded additive scalar action assignment
(+^|=) :: (RoundedMixedAddInPlace t tn) => OpNonmut t tn s
(+^|=) = mutableNonmutToNonmut mixedAddUpInPlace

-- | Downward rounded in-place mixed addition
mixedAddDnInPlace :: (RoundedMixedAddInPlace t tn) =>
    OpMutableNonmut t tn s
mixedAddDnInPlace =
    mixedEffToMutableNonmut mixedAddDnInPlaceEff mixedAddDefaultEffort

-- | Downward rounded additive scalar action assignment
(+.|=) :: (RoundedMixedAddInPlace t tn) => OpNonmut t tn s
(+.|=) = mutableNonmutToNonmut mixedAddDnInPlace

-- | Upward rounded in-place mixed multiplication
mixedMultUpInPlace :: (RoundedMixedMultiplyInPlace t tn) => 
    OpMutableNonmut t tn s
mixedMultUpInPlace =
    mixedEffToMutableNonmut mixedMultUpInPlaceEff mixedMultDefaultEffort

-- | Upward rounded multiplicative scalar action assignment
(*^|=) :: (RoundedMixedMultiplyInPlace t tn) => OpNonmut t tn s
(*^|=) = mutableNonmutToNonmut mixedMultUpInPlace

-- | Downward rounded in-place mixed multiplication
mixedMultDnInPlace :: (RoundedMixedMultiplyInPlace t tn) => 
    OpMutableNonmut t tn s
mixedMultDnInPlace =
    mixedEffToMutableNonmut mixedMultDnInPlaceEff mixedMultDefaultEffort

-- | Downward rounded multiplicative scalar action assignment
(*.|=) :: (RoundedMixedMultiplyInPlace t tn) => OpNonmut t tn s
(*.|=) = mutableNonmutToNonmut mixedMultDnInPlace

-- | Upward rounded in-place mixed reciprocal action
mixedDivUpInPlace :: (RoundedMixedDivideInPlace t tn) => 
    OpMutableNonmut t tn s
mixedDivUpInPlace =
    mixedEffToMutableNonmut mixedDivUpInPlaceEff mixedDivDefaultEffort

-- | Upward rounded multiplicative scalar reciprocal action assignment
(/^|=) :: (RoundedMixedDivideInPlace t tn) => OpNonmut t tn s
(/^|=) = mutableNonmutToNonmut mixedDivDnInPlace

-- | Downward rounded in-place mixed reciprocal action
mixedDivDnInPlace :: (RoundedMixedDivideInPlace t tn) => 
    OpMutableNonmut t tn s
mixedDivDnInPlace =
    mixedEffToMutableNonmut mixedDivDnInPlaceEff mixedDivDefaultEffort

-- | Downward rounded multiplicative scalar reciprocal action assignment
(/.|=) :: (RoundedMixedDivideInPlace t tn) => OpNonmut t tn s
(/.|=) = mutableNonmutToNonmut mixedDivDnInPlace

-- | Upward rounded in-place exponential
expUpInPlace :: (RoundedExponentiationInPlace t) => OpMutable1 t s
expUpInPlace = mutable1EffToMutable1 expUpInPlaceEff expDefaultEffort 

-- | Downward rounded in-place exponential
expDnInPlace :: (RoundedExponentiationInPlace t) => OpMutable1 t s
expDnInPlace = mutable1EffToMutable1 expDnInPlaceEff expDefaultEffort 

-- | Upward rounded in-place square root
sqrtUpInPlace :: (RoundedSquareRootInPlace t) => OpMutable1 t s
sqrtUpInPlace = mutable1EffToMutable1 sqrtUpInPlaceEff sqrtDefaultEffort 

-- | Downward rounded in-place square root
sqrtDnInPlace :: (RoundedSquareRootInPlace t) => OpMutable1 t s
sqrtDnInPlace = mutable1EffToMutable1 sqrtDnInPlaceEff sqrtDefaultEffort 






