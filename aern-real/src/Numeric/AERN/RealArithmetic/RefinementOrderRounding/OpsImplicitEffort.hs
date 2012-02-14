{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
    Description :  convenience binary infix operators with implicit effort parameters  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience binary infix operators with implicit effort parameters.
-}

module Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort where

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


(>+<), (<+>) :: 
    (RoundedAdd t, ?addInOutEffort :: AddEffortIndicator t) => 
    t -> t -> t
(>+<) = addInEff ?addInOutEffort
(<+>) = addOutEff ?addInOutEffort

(>-<), (<->) :: 
    (RoundedSubtr t, ?addInOutEffort :: AddEffortIndicator t) => 
    t -> t -> t
(>-<) = subtrInEff ?addInOutEffort
(<->) = subtrOutEff ?addInOutEffort

absIn, absOut ::
    (RoundedAbs t, ?absInOutEffort :: AbsEffortIndicator t) => 
    t -> t
absIn = absInEff ?absInOutEffort
absOut = absOutEff ?absInOutEffort

(>*<), (<*>) :: 
    (RoundedMultiply t, ?multInOutEffort :: MultEffortIndicator t) => 
    t -> t -> t
(>*<) = multInEff ?multInOutEffort
(<*>) = multOutEff ?multInOutEffort

(>^<), (<^>) :: 
    (RoundedPowerToNonnegInt t, ?intPowerInOutEffort :: PowerToNonnegIntEffortIndicator t) => 
    t -> Int -> t
(>^<) = powerToNonnegIntInEff ?intPowerInOutEffort
(<^>) = powerToNonnegIntOutEff ?intPowerInOutEffort

(>/<), (</>) :: 
    (RoundedDivide t, ?divInOutEffort :: DivEffortIndicator t) => 
    t -> t -> t
(>/<) = divInEff ?divInOutEffort
(</>) = divOutEff ?divInOutEffort

-- the following does not work, but is kept here as a template for
-- cut and pasting the "let"s
withFieldOpsEffortIndicator effortField expression =
    let ?addInOutEffort = fldEffortAdd effortField in
    let ?multInOutEffort = fldEffortMult effortField in
    let ?intPowerInOutEffort = fldEffortPow effortField in
    let ?divInOutEffort = fldEffortDiv effortField in
    expression

(|>+<), (|<+>) :: 
    (RoundedMixedAdd t tn, 
     ?mixedAddInOutEffort :: MixedAddEffortIndicator t tn) => 
    tn -> t -> t
(|>+<) n d = mixedAddInEff ?mixedAddInOutEffort d n
(|<+>) n d = mixedAddOutEff ?mixedAddInOutEffort d n

(>+<|), (<+>|) :: 
    (RoundedMixedAdd t tn, 
     ?mixedAddInOutEffort :: MixedAddEffortIndicator t tn) => 
    t -> tn -> t
(>+<|) = mixedAddInEff ?mixedAddInOutEffort
(<+>|) = mixedAddOutEff ?mixedAddInOutEffort

(|>*<), (|<*>) :: 
    (RoundedMixedMultiply t tn, 
     ?mixedMultInOutEffort :: MixedMultEffortIndicator t tn) => 
    tn -> t -> t
(|>*<) n d = mixedMultInEff ?mixedMultInOutEffort d n
(|<*>) n d = mixedMultOutEff ?mixedMultInOutEffort d n

(>*<|), (<*>|) :: 
    (RoundedMixedMultiply t tn, 
     ?mixedMultInOutEffort :: MixedMultEffortIndicator t tn) => 
    t -> tn -> t
(>*<|) = mixedMultInEff ?mixedMultInOutEffort
(<*>|) = mixedMultOutEff ?mixedMultInOutEffort

(>/<|), (</>|) :: 
    (RoundedMixedDivide t tn, 
     ?mixedDivInOutEffort :: MixedDivEffortIndicator t tn) => 
    t -> tn -> t
(>/<|) = mixedDivInEff ?mixedDivInOutEffort
(</>|) = mixedDivOutEff ?mixedDivInOutEffort

-- the following does not work, but is kept here as a template for
-- cut and pasting the "let"s
--withMixedFieldOpsEffortIndicator effortMixedField expression =
--    let ?mixedAddInOutEffort = mxfldEffortAdd effortMixedField in
--    let ?mixedMultInOutEffort = mxfldEffortMult effortMixedField in
--    let ?mixedDivInOutEffort = mxfldEffortDiv effortMixedField in
--    expression


piIn, piOut ::
    (RoundedSpecialConst t, ?specialConstInOutEffort :: SpecialConstEffortIndicator t) => 
    t
piIn = piInEff ?specialConstInOutEffort
piOut = piOutEff ?specialConstInOutEffort

eIn, eOut ::
    (RoundedSpecialConst t, ?specialConstInOutEffort :: SpecialConstEffortIndicator t) => 
    t
eIn = eInEff ?specialConstInOutEffort
eOut = eOutEff ?specialConstInOutEffort

expIn, expOut ::
    (RoundedExponentiation t, ?expInOutEffort :: ExpEffortIndicator t) => 
    t -> t
expIn = expInEff ?expInOutEffort
expOut = expOutEff ?expInOutEffort

sqrtIn, sqrtOut ::
    (RoundedSquareRoot t, ?sqrtInOutEffort :: SqrtEffortIndicator t) => 
    t -> t
sqrtIn = sqrtInEff ?sqrtInOutEffort
sqrtOut = sqrtOutEff ?sqrtInOutEffort

