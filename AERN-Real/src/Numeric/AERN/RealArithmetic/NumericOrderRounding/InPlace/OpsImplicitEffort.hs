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
    (RoundedAddInPlace t, ?addUpDnEffort :: AddEffortIndicator t) => 
    OpMutable2 t s
addUpInPlace = addUpInPlaceEff ?addUpDnEffort

-- | Upward rounded addition assignment
(+^=) :: 
    (RoundedAddInPlace t, ?addUpDnEffort :: AddEffortIndicator t) => 
    OpMutable1 t s
(+^=) = mutable2ToMutable1 addUpInPlace

-- | Downward rounded in-place addition
addDnInPlace :: 
    (RoundedAddInPlace t, ?addUpDnEffort :: AddEffortIndicator t) => 
    OpMutable2 t s
addDnInPlace = addDnInPlaceEff ?addUpDnEffort

-- | Downward rounded addition assignment
(+.=) :: 
    (RoundedAddInPlace t, ?addUpDnEffort :: AddEffortIndicator t) => 
    OpMutable1 t s
(+.=) = mutable2ToMutable1 addDnInPlace

-- | Upward rounded in-place subtraction
subtrUpInPlace :: 
    (RoundedSubtrInPlace t, ?addUpDnEffort :: AddEffortIndicator t) => 
    OpMutable2 t s
subtrUpInPlace = subtrUpInPlaceEff ?addUpDnEffort

-- | Upward rounded subtraction assignment
(-^=) :: 
    (RoundedSubtrInPlace t, ?addUpDnEffort :: AddEffortIndicator t) => 
    OpMutable1 t s
(-^=) = mutable2ToMutable1 subtrUpInPlace

-- | Downward rounded in-place subtraction
subtrDnInPlace :: 
    (RoundedSubtrInPlace t, ?addUpDnEffort :: AddEffortIndicator t) => 
    OpMutable2 t s
subtrDnInPlace = subtrDnInPlaceEff ?addUpDnEffort

-- | Downward rounded subtraction assignment
(-.=) :: 
    (RoundedSubtrInPlace t, ?addUpDnEffort :: AddEffortIndicator t) => 
    OpMutable1 t s
(-.=) = mutable2ToMutable1 subtrDnInPlace

-- | Upward rounded in-place absolute value
absUpInPlace ::
    (RoundedAbsInPlace t, ?absUpDnEffort :: AbsEffortIndicator t) => 
    OpMutable1 t s
absUpInPlace = absUpInPlaceEff ?absUpDnEffort

-- | Downward rounded in-place absolute value
absDnInPlace ::
    (RoundedAbsInPlace t, ?absUpDnEffort :: AbsEffortIndicator t) => 
    OpMutable1 t s
absDnInPlace = absDnInPlaceEff ?absUpDnEffort

-- | Upward rounded in-place multiplication
multUpInPlace :: 
    (RoundedMultiplyInPlace t, ?multUpDnEffort :: MultEffortIndicator t) => 
    OpMutable2 t s
multUpInPlace = multUpInPlaceEff ?multUpDnEffort

-- | Upward rounded multiplication assignment
(*^=) :: 
    (RoundedMultiplyInPlace t, ?multUpDnEffort :: MultEffortIndicator t) => 
    OpMutable1 t s
(*^=) = mutable2ToMutable1 multUpInPlace

-- | Downward rounded in-place multiplication
multDnInPlace :: 
    (RoundedMultiplyInPlace t, ?multUpDnEffort :: MultEffortIndicator t) => 
    OpMutable2 t s
multDnInPlace = multDnInPlaceEff ?multUpDnEffort

-- | Downward rounded multiplication assignment
(*.=) :: 
    (RoundedMultiplyInPlace t, ?multUpDnEffort :: MultEffortIndicator t) => 
    OpMutable1 t s
(*.=) = mutable2ToMutable1 multDnInPlace

-- | Upward rounded in-place power
powerToNonnegIntUpInPlace :: 
    (RoundedPowerToNonnegIntInPlace t, 
     ?intPowerUpDnEffort :: PowerToNonnegIntEffortIndicator t) => 
    OpMutableNonmut t Int s
powerToNonnegIntUpInPlace = powerToNonnegIntUpInPlaceEff ?intPowerUpDnEffort

-- | Upward rounded in-place power assignment
(^^=)  :: 
    (RoundedPowerToNonnegIntInPlace t, 
     ?intPowerUpDnEffort :: PowerToNonnegIntEffortIndicator t) => 
    OpNonmut t Int s
(^^=) = mutableNonmutToNonmut powerToNonnegIntUpInPlace

-- | Downward rounded in-place power
powerToNonnegIntDnInPlace :: 
    (RoundedPowerToNonnegIntInPlace t, 
     ?intPowerUpDnEffort :: PowerToNonnegIntEffortIndicator t) => 
    OpMutableNonmut t Int s
powerToNonnegIntDnInPlace = powerToNonnegIntDnInPlaceEff ?intPowerUpDnEffort

-- | Upward rounded in-place power assignment
(^.=)  :: 
    (RoundedPowerToNonnegIntInPlace t, 
     ?intPowerUpDnEffort :: PowerToNonnegIntEffortIndicator t) => 
    OpNonmut t Int s
(^.=) = mutableNonmutToNonmut powerToNonnegIntDnInPlace

-- | Upward rounded in-place division
divUpInPlace :: 
    (RoundedDivideInPlace t, ?divUpDnEffort :: DivEffortIndicator t) => 
    OpMutable2 t s
divUpInPlace = divUpInPlaceEff ?divUpDnEffort

-- | Upward rounded division assignment
(/^=) :: 
    (RoundedDivideInPlace t, ?divUpDnEffort :: DivEffortIndicator t) => 
    OpMutable1 t s
(/^=) = mutable2ToMutable1 divUpInPlace

-- | Downward rounded in-place division
divDnInPlace :: 
    (RoundedDivideInPlace t, ?divUpDnEffort :: DivEffortIndicator t) => 
    OpMutable2 t s
divDnInPlace = divDnInPlaceEff ?divUpDnEffort

-- | Downward rounded division assignment
(/.=) :: 
    (RoundedDivideInPlace t, ?divUpDnEffort :: DivEffortIndicator t) => 
    OpMutable1 t s
(/.=) = mutable2ToMutable1 divDnInPlace

-- the following does not work, but is kept here as a template for
-- cut and pasting the "let"s
withFieldOpsEffortIndicator effortField expression =
    let ?addUpDnEffort = fldEffortAdd effortField in
    let ?multUpDnEffort = fldEffortMult effortField in
    let ?intPowerUpDnEffort = fldEffortPow effortField in
    let ?divUpDnEffort = fldEffortDiv effortField in
    expression

-- | Upward rounded in-place mixed addition
mixedAddUpInPlace :: 
    (RoundedMixedAddInPlace t tn, 
     ?mixedAddUpDnEffort :: MixedAddEffortIndicator t tn) => 
    OpMutableNonmut t tn s
mixedAddUpInPlace = mixedAddUpInPlaceEff ?mixedAddUpDnEffort

-- | Upward rounded additive scalar action assignment
(+^|=) :: 
    (RoundedMixedAddInPlace t tn, 
     ?mixedAddUpDnEffort :: MixedAddEffortIndicator t tn) => 
    OpNonmut t tn s
(+^|=) = mutableNonmutToNonmut mixedAddUpInPlace

-- | Downward rounded in-place mixed addition
mixedAddDnInPlace :: 
    (RoundedMixedAddInPlace t tn, 
     ?mixedAddUpDnEffort :: MixedAddEffortIndicator t tn) => 
    OpMutableNonmut t tn s
mixedAddDnInPlace = mixedAddDnInPlaceEff ?mixedAddUpDnEffort

-- | Downward rounded additive scalar action assignment
(+.|=) :: 
    (RoundedMixedAddInPlace t tn, 
     ?mixedAddUpDnEffort :: MixedAddEffortIndicator t tn) => 
    OpNonmut t tn s
(+.|=) = mutableNonmutToNonmut mixedAddDnInPlace

-- | Upward rounded in-place mixed multiplication
mixedMultUpInPlace :: 
    (RoundedMixedMultiplyInPlace t tn, 
     ?mixedMultUpDnEffort :: MixedMultEffortIndicator t tn) => 
    OpMutableNonmut t tn s
mixedMultUpInPlace = mixedMultUpInPlaceEff ?mixedMultUpDnEffort

-- | Upward rounded multiplicative scalar action assignment
(*^|=) :: 
    (RoundedMixedMultiplyInPlace t tn, 
     ?mixedMultUpDnEffort :: MixedMultEffortIndicator t tn) => 
    OpNonmut t tn s
(*^|=) = mutableNonmutToNonmut mixedMultUpInPlace

-- | Downward rounded in-place mixed multiplication
mixedMultDnInPlace :: 
    (RoundedMixedMultiplyInPlace t tn, 
     ?mixedMultUpDnEffort :: MixedMultEffortIndicator t tn) => 
    OpMutableNonmut t tn s
mixedMultDnInPlace = mixedMultDnInPlaceEff ?mixedMultUpDnEffort

-- | Downward rounded multiplicative scalar action assignment
(*.|=) :: 
    (RoundedMixedMultiplyInPlace t tn, 
     ?mixedMultUpDnEffort :: MixedMultEffortIndicator t tn) => 
    OpNonmut t tn s
(*.|=) = mutableNonmutToNonmut mixedMultDnInPlace

-- | Upward rounded in-place mixed reciprocal action
mixedDivUpInPlace :: 
    (RoundedMixedDivideInPlace t tn, 
     ?mixedDivUpDnEffort :: MixedDivEffortIndicator t tn) => 
    OpMutableNonmut t tn s
mixedDivUpInPlace = mixedDivUpInPlaceEff ?mixedDivUpDnEffort

-- | Upward rounded multiplicative scalar reciprocal action assignment
(/^|=) :: 
    (RoundedMixedDivideInPlace t tn, 
     ?mixedDivUpDnEffort :: MixedDivEffortIndicator t tn) => 
    OpNonmut t tn s
(/^|=) = mutableNonmutToNonmut mixedDivUpInPlace

-- | Downward rounded in-place mixed reciprocal action
mixedDivDnInPlace :: 
    (RoundedMixedDivideInPlace t tn, 
     ?mixedDivUpDnEffort :: MixedDivEffortIndicator t tn) => 
    OpMutableNonmut t tn s
mixedDivDnInPlace = mixedDivDnInPlaceEff ?mixedDivUpDnEffort

-- | Downward rounded multiplicative scalar reciprocal action assignment
(/.|=) :: 
    (RoundedMixedDivideInPlace t tn, 
     ?mixedDivUpDnEffort :: MixedDivEffortIndicator t tn) => 
    OpNonmut t tn s
(/.|=) = mutableNonmutToNonmut mixedDivDnInPlace

-- the following does not work, but is kept here as a template for
-- cut and pasting the "let"s
withMixedFieldOpsEffortIndicator effortMixedField expression =
    let ?mixedAddUpDnEffort = mxfldEffortAdd effortMixedField in
    let ?mixedMultUpDnEffort = mxfldEffortMult effortMixedField in
    let ?mixedDivUpDnEffort = mxfldEffortDiv effortMixedField in
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

