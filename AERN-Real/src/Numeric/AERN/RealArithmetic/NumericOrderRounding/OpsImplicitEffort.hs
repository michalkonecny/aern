{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsImplicitEffort
    Description :  convenience binary infix operators with implicit effort parameters  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience binary infix operators with implicit effort parameters.
-}

module Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsImplicitEffort where

import Numeric.AERN.RealArithmetic.NumericOrderRounding

infixl 6 +., +^, -., -^
infixl 7 *., *^
infixl 8 ^., ^^
infixl 7 /., /^

infixr 6 |+., |+^
infixl 6 +.|, +^|
infixr 7 |*., |*^
infixl 7 *.|, *^|
infixl 7 /.|, /^|

(+^), (+.) :: 
    (RoundedAdd t, ?addUpDnEffort :: AddEffortIndicator t) => 
    t -> t -> t
(+^) = addUpEff ?addUpDnEffort
(+.) = addDnEff ?addUpDnEffort

(-^), (-.) :: 
    (RoundedSubtr t, ?addUpDnEffort :: AddEffortIndicator t) => 
    t -> t -> t
(-^) = subtrUpEff ?addUpDnEffort
(-.) = subtrDnEff ?addUpDnEffort

absUp, absDn ::
    (RoundedAbs t, ?absUpDnEffort :: AbsEffortIndicator t) => 
    t -> t
absUp = absUpEff ?absUpDnEffort
absDn = absDnEff ?absUpDnEffort

(*^), (*.) :: 
    (RoundedMultiply t, ?multUpDnEffort :: MultEffortIndicator t) => 
    t -> t -> t
(*^) = multUpEff ?multUpDnEffort
(*.) = multDnEff ?multUpDnEffort

(^^), (^.) :: 
    (RoundedPowerToNonnegInt t, ?intPowerUpDnEffort :: PowerToNonnegIntEffortIndicator t) => 
    t -> Int -> t
(^^) = powerToNonnegIntUpEff ?intPowerUpDnEffort
(^.) = powerToNonnegIntDnEff ?intPowerUpDnEffort

(/^), (/.) :: 
    (RoundedDivide t, ?divUpDnEffort :: DivEffortIndicator t) => 
    t -> t -> t
(/^) = divUpEff ?divUpDnEffort
(/.) = divDnEff ?divUpDnEffort

(+^|), (+.|) ::
    (RoundedMixedAdd t tn, 
     ?mixedAddUpDnEffort :: MixedAddEffortIndicator t tn) => 
    t -> tn -> t
(+^|) = mixedAddUpEff ?mixedAddUpDnEffort
(+.|) = mixedAddDnEff ?mixedAddUpDnEffort

(|+^), (|+.) ::
    (RoundedMixedAdd t tn, 
     ?mixedAddUpDnEffort :: MixedAddEffortIndicator t tn) => 
    tn -> t -> t
(|+^) n d = mixedAddUpEff ?mixedAddUpDnEffort d n
(|+.) n d = mixedAddDnEff ?mixedAddUpDnEffort d n

(*^|), (*.|) ::
    (RoundedMixedMultiply t tn, 
     ?mixedMultUpDnEffort :: MixedMultEffortIndicator t tn) => 
    t -> tn -> t
(*^|) = mixedMultUpEff ?mixedMultUpDnEffort
(*.|) = mixedMultDnEff ?mixedMultUpDnEffort

(|*^), (|*.) ::
    (RoundedMixedMultiply t tn, 
     ?mixedMultUpDnEffort :: MixedMultEffortIndicator t tn) => 
    tn -> t -> t
(|*^) n d = mixedMultUpEff ?mixedMultUpDnEffort d n
(|*.) n d = mixedMultDnEff ?mixedMultUpDnEffort d n

(/^|), (/.|) ::
    (RoundedMixedDivide t tn, 
     ?mixedDivUpDnEffort :: MixedDivEffortIndicator t tn) => 
    t -> tn -> t
(/^|) = mixedDivUpEff ?mixedDivUpDnEffort
(/.|) = mixedDivDnEff ?mixedDivUpDnEffort

piUp, piDn ::
    (RoundedSpecialConst t, ?specialConstUpDnEffort :: SpecialConstEffortIndicator t) => 
    t
piUp = piUpEff ?specialConstUpDnEffort
piDn = piDnEff ?specialConstUpDnEffort

eUp, eDn ::
    (RoundedSpecialConst t, ?specialConstUpDnEffort :: SpecialConstEffortIndicator t) => 
    t
eUp = eUpEff ?specialConstUpDnEffort
eDn = eDnEff ?specialConstUpDnEffort

expUp, expDn ::
    (RoundedExponentiation t, ?expUpDnEffort :: ExpEffortIndicator t) => 
    t -> t
expUp = expUpEff ?expUpDnEffort
expDn = expDnEff ?expUpDnEffort

sqrtUp, sqrtDn ::
    (RoundedSquareRoot t, ?sqrtUpDnEffort :: SqrtEffortIndicator t) => 
    t -> t
sqrtUp = sqrtUpEff ?sqrtUpDnEffort
sqrtDn = sqrtDnEff ?sqrtUpDnEffort

