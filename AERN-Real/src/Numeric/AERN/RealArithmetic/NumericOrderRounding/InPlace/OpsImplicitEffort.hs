{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.OpsImplicitEffort
    Description :  convenience directed-rounded in-place operators and functions with implicit effort parameters  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience directed-rounded in-place operators and functions with implicit effort parameters.
-}

module Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.OpsImplicitEffort where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.RealArithmetic.NumericOrderRounding

-- | Upward rounded in-place addition
addUpInPlace :: 
    (RoundedAddInPlace t, ?addInOutEffort :: AddEffortIndicator t) => 
    OpMutable2 t s
addUpInPlace = addUpInPlaceEff ?addInOutEffort

-- | Upward rounded addition assignment
(+^=) :: 
    (RoundedAddInPlace t, ?addInOutEffort :: AddEffortIndicator t) => 
    OpMutable1 t s
(+^=) = mutable2ToMutable1 addUpInPlace

-- | Downward rounded in-place addition
addDnInPlace :: 
    (RoundedAddInPlace t, ?addInOutEffort :: AddEffortIndicator t) => 
    OpMutable2 t s
addDnInPlace = addDnInPlaceEff ?addInOutEffort

-- | Downward rounded addition assignment
(+.=) :: 
    (RoundedAddInPlace t, ?addInOutEffort :: AddEffortIndicator t) => 
    OpMutable1 t s
(+.=) = mutable2ToMutable1 addDnInPlace

-- | Upward rounded in-place subtraction
subtrUpInPlace :: 
    (RoundedSubtrInPlace t, ?addInOutEffort :: AddEffortIndicator t) => 
    OpMutable2 t s
subtrUpInPlace = subtrUpInPlaceEff ?addInOutEffort

-- | Upward rounded subtraction assignment
(-^=) :: 
    (RoundedSubtrInPlace t, ?addInOutEffort :: AddEffortIndicator t) => 
    OpMutable1 t s
(-^=) = mutable2ToMutable1 subtrUpInPlace

-- | Downward rounded in-place subtraction
subtrDnInPlace :: 
    (RoundedSubtrInPlace t, ?addInOutEffort :: AddEffortIndicator t) => 
    OpMutable2 t s
subtrDnInPlace = subtrDnInPlaceEff ?addInOutEffort

-- | Downward rounded subtraction assignment
(-.=) :: 
    (RoundedSubtrInPlace t, ?addInOutEffort :: AddEffortIndicator t) => 
    OpMutable1 t s
(-.=) = mutable2ToMutable1 subtrDnInPlace

-- | Upward rounded in-place absolute value
absUpInPlace ::
    (RoundedAbsInPlace t, ?absInOutEffort :: AbsEffortIndicator t) => 
    OpMutable1 t s
absUpInPlace = absUpInPlaceEff ?absInOutEffort

-- | Downward rounded in-place absolute value
absDnInPlace ::
    (RoundedAbsInPlace t, ?absInOutEffort :: AbsEffortIndicator t) => 
    OpMutable1 t s
absDnInPlace = absDnInPlaceEff ?absInOutEffort

-- | Upward rounded in-place multiplication
multUpInPlace :: 
    (RoundedMultiplyInPlace t, ?multInOutEffort :: MultEffortIndicator t) => 
    OpMutable2 t s
multUpInPlace = multUpInPlaceEff ?multInOutEffort

-- | Upward rounded multiplication assignment
(*^=) :: 
    (RoundedMultiplyInPlace t, ?multInOutEffort :: MultEffortIndicator t) => 
    OpMutable1 t s
(*^=) = mutable2ToMutable1 multUpInPlace

-- | Downward rounded in-place multiplication
multDnInPlace :: 
    (RoundedMultiplyInPlace t, ?multInOutEffort :: MultEffortIndicator t) => 
    OpMutable2 t s
multDnInPlace = multDnInPlaceEff ?multInOutEffort

-- | Downward rounded multiplication assignment
(*.=) :: 
    (RoundedMultiplyInPlace t, ?multInOutEffort :: MultEffortIndicator t) => 
    OpMutable1 t s
(*.=) = mutable2ToMutable1 multDnInPlace

-- | Upward rounded in-place power
powerToNonnegIntUpInPlace :: 
    (RoundedPowerToNonnegIntInPlace t, 
     ?intPowerInOutEffort :: PowerToNonnegIntEffortIndicator t) => 
    OpMutableNonmut t Int s
powerToNonnegIntUpInPlace = powerToNonnegIntUpInPlaceEff ?intPowerInOutEffort

-- | Upward rounded in-place power assignment
(^^=)  :: 
    (RoundedPowerToNonnegIntInPlace t, 
     ?intPowerInOutEffort :: PowerToNonnegIntEffortIndicator t) => 
    OpNonmut t Int s
(^^=) = mutableNonmutToNonmut powerToNonnegIntUpInPlace

-- | Downward rounded in-place power
powerToNonnegIntDnInPlace :: 
    (RoundedPowerToNonnegIntInPlace t, 
     ?intPowerInOutEffort :: PowerToNonnegIntEffortIndicator t) => 
    OpMutableNonmut t Int s
powerToNonnegIntDnInPlace = powerToNonnegIntDnInPlaceEff ?intPowerInOutEffort

-- | Upward rounded in-place power assignment
(^.=)  :: 
    (RoundedPowerToNonnegIntInPlace t, 
     ?intPowerInOutEffort :: PowerToNonnegIntEffortIndicator t) => 
    OpNonmut t Int s
(^.=) = mutableNonmutToNonmut powerToNonnegIntDnInPlace

-- | Upward rounded in-place division
divUpInPlace :: 
    (RoundedDivideInPlace t, ?divInOutEffort :: DivEffortIndicator t) => 
    OpMutable2 t s
divUpInPlace = divUpInPlaceEff ?divInOutEffort

-- | Upward rounded division assignment
(/^=) :: 
    (RoundedDivideInPlace t, ?divInOutEffort :: DivEffortIndicator t) => 
    OpMutable1 t s
(/^=) = mutable2ToMutable1 divUpInPlace

-- | Downward rounded in-place division
divDnInPlace :: 
    (RoundedDivideInPlace t, ?divInOutEffort :: DivEffortIndicator t) => 
    OpMutable2 t s
divDnInPlace = divDnInPlaceEff ?divInOutEffort

-- | Downward rounded division assignment
(/.=) :: 
    (RoundedDivideInPlace t, ?divInOutEffort :: DivEffortIndicator t) => 
    OpMutable1 t s
(/.=) = mutable2ToMutable1 divDnInPlace

-- the following does not work, but is kept here as a template for
-- cut and pasting the "let"s
withFieldOpsEffortIndicator effortField expression =
    let ?addInOutEffort = fldEffortAdd effortField in
    let ?multInOutEffort = fldEffortMult effortField in
    let ?intPowerInOutEffort = fldEffortPow effortField in
    let ?divInOutEffort = fldEffortDiv effortField in
    expression

-- | Upward rounded in-place mixed addition
mixedAddUpInPlace :: 
    (RoundedMixedAddInPlace t tn, 
     ?mixedAddInOutEffort :: MixedAddEffortIndicator t tn) => 
    OpMutableNonmut t tn s
mixedAddUpInPlace = mixedAddUpInPlaceEff ?mixedAddInOutEffort

-- | Upward rounded additive scalar action assignment
(+^|=) :: 
    (RoundedMixedAddInPlace t tn, 
     ?mixedAddInOutEffort :: MixedAddEffortIndicator t tn) => 
    OpNonmut t tn s
(+^|=) = mutableNonmutToNonmut mixedAddUpInPlace

-- | Downward rounded in-place mixed addition
mixedAddDnInPlace :: 
    (RoundedMixedAddInPlace t tn, 
     ?mixedAddInOutEffort :: MixedAddEffortIndicator t tn) => 
    OpMutableNonmut t tn s
mixedAddDnInPlace = mixedAddDnInPlaceEff ?mixedAddInOutEffort

-- | Downward rounded additive scalar action assignment
(+.|=) :: 
    (RoundedMixedAddInPlace t tn, 
     ?mixedAddInOutEffort :: MixedAddEffortIndicator t tn) => 
    OpNonmut t tn s
(+.|=) = mutableNonmutToNonmut mixedAddDnInPlace

-- | Upward rounded in-place mixed multiplication
mixedMultUpInPlace :: 
    (RoundedMixedMultiplyInPlace t tn, 
     ?mixedMultInOutEffort :: MixedMultEffortIndicator t tn) => 
    OpMutableNonmut t tn s
mixedMultUpInPlace = mixedMultUpInPlaceEff ?mixedMultInOutEffort

-- | Upward rounded multiplicative scalar action assignment
(*^|=) :: 
    (RoundedMixedMultiplyInPlace t tn, 
     ?mixedMultInOutEffort :: MixedMultEffortIndicator t tn) => 
    OpNonmut t tn s
(*^|=) = mutableNonmutToNonmut mixedMultUpInPlace

-- | Downward rounded in-place mixed multiplication
mixedMultDnInPlace :: 
    (RoundedMixedMultiplyInPlace t tn, 
     ?mixedMultInOutEffort :: MixedMultEffortIndicator t tn) => 
    OpMutableNonmut t tn s
mixedMultDnInPlace = mixedMultDnInPlaceEff ?mixedMultInOutEffort

-- | Downward rounded multiplicative scalar action assignment
(*.|=) :: 
    (RoundedMixedMultiplyInPlace t tn, 
     ?mixedMultInOutEffort :: MixedMultEffortIndicator t tn) => 
    OpNonmut t tn s
(*.|=) = mutableNonmutToNonmut mixedMultDnInPlace

-- | Upward rounded in-place mixed reciprocal action
mixedDivUpInPlace :: 
    (RoundedMixedDivideInPlace t tn, 
     ?mixedDivInOutEffort :: MixedDivEffortIndicator t tn) => 
    OpMutableNonmut t tn s
mixedDivUpInPlace = mixedDivUpInPlaceEff ?mixedDivInOutEffort

-- | Upward rounded multiplicative scalar reciprocal action assignment
(/^|=) :: 
    (RoundedMixedDivideInPlace t tn, 
     ?mixedDivInOutEffort :: MixedDivEffortIndicator t tn) => 
    OpNonmut t tn s
(/^|=) = mutableNonmutToNonmut mixedDivUpInPlace

-- | Downward rounded in-place mixed reciprocal action
mixedDivDnInPlace :: 
    (RoundedMixedDivideInPlace t tn, 
     ?mixedDivInOutEffort :: MixedDivEffortIndicator t tn) => 
    OpMutableNonmut t tn s
mixedDivDnInPlace = mixedDivDnInPlaceEff ?mixedDivInOutEffort

-- | Downward rounded multiplicative scalar reciprocal action assignment
(/.|=) :: 
    (RoundedMixedDivideInPlace t tn, 
     ?mixedDivInOutEffort :: MixedDivEffortIndicator t tn) => 
    OpNonmut t tn s
(/.|=) = mutableNonmutToNonmut mixedDivDnInPlace

-- the following does not work, but is kept here as a template for
-- cut and pasting the "let"s
withMixedFieldOpsEffortIndicator effortMixedField expression =
    let ?mixedAddInOutEffort = mxfldEffortAdd effortMixedField in
    let ?mixedMultInOutEffort = mxfldEffortMult effortMixedField in
    let ?mixedDivInOutEffort = mxfldEffortDiv effortMixedField in
    expression

-- | Upward rounded in-place exponential
expUpInPlace ::
    (RoundedExponentiationInPlace t, ?expUpDnEffort :: ExpEffortIndicator t) => 
    OpMutable1 t s
expUpInPlace = expUpInPlaceEff ?expUpDnEffort

-- | Downward rounded in-place exponential
expDnInPlace ::
    (RoundedExponentiationInPlace t, ?expUpDnEffort :: ExpEffortIndicator t) => 
    OpMutable1 t s
expDnInPlace = expDnInPlaceEff ?expUpDnEffort

-- | Upward rounded in-place square root
sqrtUpInPlace ::
    (RoundedSquareRootInPlace t, ?sqrtUpDnEffort :: SqrtEffortIndicator t) => 
    OpMutable1 t s
sqrtUpInPlace = sqrtUpInPlaceEff ?sqrtUpDnEffort

-- | Downward rounded in-place square root
sqrtDnInPlace ::
    (RoundedSquareRootInPlace t, ?sqrtUpDnEffort :: SqrtEffortIndicator t) => 
    OpMutable1 t s
sqrtDnInPlace = sqrtDnInPlaceEff ?sqrtUpDnEffort

