{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.OpsImplicitEffort
    Description :  onvenience directed-rounded in-place operators and functions with implicit effort parameters  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience directed-rounded in-place operators and functions with implicit effort parameters.
-}

module Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.OpsImplicitEffort where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.RealArithmetic.RefinementOrderRounding

-- | Inward rounded in-place addition
addInInPlace :: 
    (RoundedAddInPlace t, ?addInOutEffort :: AddEffortIndicator t) => 
    OpMutable2 t s
addInInPlace = addInInPlaceEff ?addInOutEffort

-- | Inward rounded addition assignment
(>+<=) :: 
    (RoundedAddInPlace t, ?addInOutEffort :: AddEffortIndicator t) => 
    OpMutable1 t s
(>+<=) = mutable2ToMutable1 addInInPlace

-- | Outward rounded in-place addition
addOutInPlace :: 
    (RoundedAddInPlace t, ?addInOutEffort :: AddEffortIndicator t) => 
    OpMutable2 t s
addOutInPlace = addOutInPlaceEff ?addInOutEffort

-- | Outward rounded addition assignment
(<+>=) :: 
    (RoundedAddInPlace t, ?addInOutEffort :: AddEffortIndicator t) => 
    OpMutable1 t s
(<+>=) = mutable2ToMutable1 addOutInPlace

-- | Inward rounded in-place subtraction
subtrInInPlace :: 
    (RoundedSubtrInPlace t, ?addInOutEffort :: AddEffortIndicator t) => 
    OpMutable2 t s
subtrInInPlace = subtrInInPlaceEff ?addInOutEffort

-- | Inward rounded subtraction assignment
(>-<=) :: 
    (RoundedSubtrInPlace t, ?addInOutEffort :: AddEffortIndicator t) => 
    OpMutable1 t s
(>-<=) = mutable2ToMutable1 subtrInInPlace

-- | Outward rounded in-place subtraction
subtrOutInPlace :: 
    (RoundedSubtrInPlace t, ?addInOutEffort :: AddEffortIndicator t) => 
    OpMutable2 t s
subtrOutInPlace = subtrOutInPlaceEff ?addInOutEffort

-- | Outward rounded subtraction assignment
(<->=) :: 
    (RoundedSubtrInPlace t, ?addInOutEffort :: AddEffortIndicator t) => 
    OpMutable1 t s
(<->=) = mutable2ToMutable1 subtrOutInPlace

-- | Inward rounded in-place absolute value
absInInPlace ::
    (RoundedAbsInPlace t, ?absInOutEffort :: AbsEffortIndicator t) => 
    OpMutable1 t s
absInInPlace = absInInPlaceEff ?absInOutEffort

-- | Outward rounded in-place absolute value
absOutInPlace ::
    (RoundedAbsInPlace t, ?absInOutEffort :: AbsEffortIndicator t) => 
    OpMutable1 t s
absOutInPlace = absOutInPlaceEff ?absInOutEffort

-- | Inward rounded in-place multiplication
multInInPlace :: 
    (RoundedMultiplyInPlace t, ?multInOutEffort :: MultEffortIndicator t) => 
    OpMutable2 t s
multInInPlace = multInInPlaceEff ?multInOutEffort

-- | Inward rounded multiplication assignment
(>*<=) :: 
    (RoundedMultiplyInPlace t, ?multInOutEffort :: MultEffortIndicator t) => 
    OpMutable1 t s
(>*<=) = mutable2ToMutable1 multInInPlace

-- | Outward rounded in-place multiplication
multOutInPlace :: 
    (RoundedMultiplyInPlace t, ?multInOutEffort :: MultEffortIndicator t) => 
    OpMutable2 t s
multOutInPlace = multOutInPlaceEff ?multInOutEffort

-- | Outward rounded multiplication assignment
(<*>=) :: 
    (RoundedMultiplyInPlace t, ?multInOutEffort :: MultEffortIndicator t) => 
    OpMutable1 t s
(<*>=) = mutable2ToMutable1 multOutInPlace

-- | Inward rounded in-place power
powerToNonnegIntInInPlace :: 
    (RoundedPowerToNonnegIntInPlace t, 
     ?intPowerInOutEffort :: PowerToNonnegIntEffortIndicator t) => 
    OpMutableNonmut t Int s
powerToNonnegIntInInPlace = powerToNonnegIntInInPlaceEff ?intPowerInOutEffort

-- | Inward rounded in-place power assignment
(>^<=)  :: 
    (RoundedPowerToNonnegIntInPlace t, 
     ?intPowerInOutEffort :: PowerToNonnegIntEffortIndicator t) => 
    OpNonmut t Int s
(>^<=) = mutableNonmutToNonmut powerToNonnegIntInInPlace

-- | Outward rounded in-place power
powerToNonnegIntOutInPlace :: 
    (RoundedPowerToNonnegIntInPlace t, 
     ?intPowerInOutEffort :: PowerToNonnegIntEffortIndicator t) => 
    OpMutableNonmut t Int s
powerToNonnegIntOutInPlace = powerToNonnegIntOutInPlaceEff ?intPowerInOutEffort

-- | Inward rounded in-place power assignment
(<^>=)  :: 
    (RoundedPowerToNonnegIntInPlace t, 
     ?intPowerInOutEffort :: PowerToNonnegIntEffortIndicator t) => 
    OpNonmut t Int s
(<^>=) = mutableNonmutToNonmut powerToNonnegIntOutInPlace

-- | Inward rounded in-place division
divInInPlace :: 
    (RoundedDivideInPlace t, ?divInOutEffort :: DivEffortIndicator t) => 
    OpMutable2 t s
divInInPlace = divInInPlaceEff ?divInOutEffort

-- | Inward rounded division assignment
(>/<=) :: 
    (RoundedDivideInPlace t, ?divInOutEffort :: DivEffortIndicator t) => 
    OpMutable1 t s
(>/<=) = mutable2ToMutable1 divInInPlace

-- | Outward rounded in-place division
divOutInPlace :: 
    (RoundedDivideInPlace t, ?divInOutEffort :: DivEffortIndicator t) => 
    OpMutable2 t s
divOutInPlace = divOutInPlaceEff ?divInOutEffort

-- | Outward rounded division assignment
(</>=) :: 
    (RoundedDivideInPlace t, ?divInOutEffort :: DivEffortIndicator t) => 
    OpMutable1 t s
(</>=) = mutable2ToMutable1 divOutInPlace

-- the following does not work, but is kept here as a template for
-- cut and pasting the "let"s
withFieldOpsEffortIndicator effortField expression =
    let ?addInOutEffort = fldEffortAdd effortField in
    let ?multInOutEffort = fldEffortMult effortField in
    let ?intPowerInOutEffort = fldEffortPow effortField in
    let ?divInOutEffort = fldEffortDiv effortField in
    expression

-- | Inward rounded in-place mixed addition
mixedAddInInPlace :: 
    (RoundedMixedAddInPlace t tn, 
     ?mixedAddInOutEffort :: MixedAddEffortIndicator t tn) => 
    OpMutableNonmut t tn s
mixedAddInInPlace = mixedAddInInPlaceEff ?mixedAddInOutEffort

-- | Inward rounded additive scalar action assignment
(>+<|=) :: 
    (RoundedMixedAddInPlace t tn, 
     ?mixedAddInOutEffort :: MixedAddEffortIndicator t tn) => 
    OpNonmut t tn s
(>+<|=) = mutableNonmutToNonmut mixedAddInInPlace

-- | Outward rounded in-place mixed addition
mixedAddOutInPlace :: 
    (RoundedMixedAddInPlace t tn, 
     ?mixedAddInOutEffort :: MixedAddEffortIndicator t tn) => 
    OpMutableNonmut t tn s
mixedAddOutInPlace = mixedAddOutInPlaceEff ?mixedAddInOutEffort

-- | Outward rounded additive scalar action assignment
(<+>|=) :: 
    (RoundedMixedAddInPlace t tn, 
     ?mixedAddInOutEffort :: MixedAddEffortIndicator t tn) => 
    OpNonmut t tn s
(<+>|=) = mutableNonmutToNonmut mixedAddOutInPlace

-- | Inward rounded in-place mixed multiplication
mixedMultInInPlace :: 
    (RoundedMixedMultiplyInPlace t tn, 
     ?mixedMultInOutEffort :: MixedMultEffortIndicator t tn) => 
    OpMutableNonmut t tn s
mixedMultInInPlace = mixedMultInInPlaceEff ?mixedMultInOutEffort

-- | Inward rounded multiplicative scalar action assignment
(>*<|=) :: 
    (RoundedMixedMultiplyInPlace t tn, 
     ?mixedMultInOutEffort :: MixedMultEffortIndicator t tn) => 
    OpNonmut t tn s
(>*<|=) = mutableNonmutToNonmut mixedMultInInPlace

-- | Outward rounded in-place mixed multiplication
mixedMultOutInPlace :: 
    (RoundedMixedMultiplyInPlace t tn, 
     ?mixedMultInOutEffort :: MixedMultEffortIndicator t tn) => 
    OpMutableNonmut t tn s
mixedMultOutInPlace = mixedMultOutInPlaceEff ?mixedMultInOutEffort

-- | Outward rounded multiplicative scalar action assignment
(<*>|=) :: 
    (RoundedMixedMultiplyInPlace t tn, 
     ?mixedMultInOutEffort :: MixedMultEffortIndicator t tn) => 
    OpNonmut t tn s
(<*>|=) = mutableNonmutToNonmut mixedMultOutInPlace

-- | Inward rounded in-place mixed reciprocal action
mixedDivInInPlace :: 
    (RoundedMixedDivideInPlace t tn, 
     ?mixedDivInOutEffort :: MixedDivEffortIndicator t tn) => 
    OpMutableNonmut t tn s
mixedDivInInPlace = mixedDivInInPlaceEff ?mixedDivInOutEffort

-- | Inward rounded multiplicative scalar reciprocal action assignment
(>/<|=) :: 
    (RoundedMixedDivideInPlace t tn, 
     ?mixedDivInOutEffort :: MixedDivEffortIndicator t tn) => 
    OpNonmut t tn s
(>/<|=) = mutableNonmutToNonmut mixedDivInInPlace

-- | Outward rounded in-place mixed reciprocal action
mixedDivOutInPlace :: 
    (RoundedMixedDivideInPlace t tn, 
     ?mixedDivInOutEffort :: MixedDivEffortIndicator t tn) => 
    OpMutableNonmut t tn s
mixedDivOutInPlace = mixedDivOutInPlaceEff ?mixedDivInOutEffort

-- | Outward rounded multiplicative scalar reciprocal action assignment
(</>|=) :: 
    (RoundedMixedDivideInPlace t tn, 
     ?mixedDivInOutEffort :: MixedDivEffortIndicator t tn) => 
    OpNonmut t tn s
(</>|=) = mutableNonmutToNonmut mixedDivOutInPlace

-- the following does not work, but is kept here as a template for
-- cut and pasting the "let"s
withMixedFieldOpsEffortIndicator effortMixedField expression =
    let ?mixedAddInOutEffort = mxfldEffortAdd effortMixedField in
    let ?mixedMultInOutEffort = mxfldEffortMult effortMixedField in
    let ?mixedDivInOutEffort = mxfldEffortDiv effortMixedField in
    expression

-- | Inward rounded in-place exponential
expInInPlace ::
    (RoundedExponentiationInPlace t, ?expInOutEffort :: ExpEffortIndicator t) => 
    OpMutable1 t s
expInInPlace = expInInPlaceEff ?expInOutEffort

-- | Outward rounded in-place exponential
expOutInPlace ::
    (RoundedExponentiationInPlace t, ?expInOutEffort :: ExpEffortIndicator t) => 
    OpMutable1 t s
expOutInPlace = expOutInPlaceEff ?expInOutEffort

-- | Inward rounded in-place square root
sqrtInInPlace ::
    (RoundedSquareRootInPlace t, ?sqrtInOutEffort :: SqrtEffortIndicator t) => 
    OpMutable1 t s
sqrtInInPlace = sqrtInInPlaceEff ?sqrtInOutEffort

-- | Outward rounded in-place square root
sqrtOutInPlace ::
    (RoundedSquareRootInPlace t, ?sqrtInOutEffort :: SqrtEffortIndicator t) => 
    OpMutable1 t s
sqrtOutInPlace = sqrtOutInPlaceEff ?sqrtInOutEffort
