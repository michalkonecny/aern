{-|
    Module      :  Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsImplicitEffort
    Description :  convenience binary infix operators with default effort  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience binary infix operators with default effort.
-}

module Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsDefaultEffort where

import Numeric.AERN.RealArithmetic.NumericOrderRounding
import Numeric.AERN.RealArithmetic.ExactOps

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
    (RoundedAdd t) => 
    t -> t -> t
(+^) d = addUpEff (addDefaultEffort d) d
(+.) d = addDnEff (addDefaultEffort d) d

(-^), (-.) :: 
    (RoundedSubtr t) => 
    t -> t -> t
(-^) d = subtrUpEff (addDefaultEffort d) d
(-.) d = subtrDnEff (addDefaultEffort d) d

absUp, absDn ::
    (RoundedAbs t) => 
    t -> t
absUp d = absUpEff (absDefaultEffort d) d
absDn d = absDnEff (absDefaultEffort d) d

(*^), (*.) :: 
    (RoundedMultiply t) => 
    t -> t -> t
(*^) d = multUpEff (multDefaultEffort d) d
(*.) d = multDnEff (multDefaultEffort d) d

(^^), (^.) :: 
    (RoundedPowerToNonnegInt t) => 
    t -> Int -> t
(^^) d = powerToNonnegIntUpEff (powerToNonnegIntDefaultEffort d) d
(^.) d = powerToNonnegIntDnEff (powerToNonnegIntDefaultEffort d) d

(/^), (/.) :: 
    (RoundedDivide t) => 
    t -> t -> t
(/^) d = divUpEff (divDefaultEffort d) d
(/.) d = divDnEff (divDefaultEffort d) d

(|+^), (|+.) :: 
    (RoundedMixedAdd t tn) => 
    tn -> t -> t
(|+^) n d = mixedAddUpEff (mixedAddDefaultEffort d n) d n
(|+.) n d = mixedAddDnEff (mixedAddDefaultEffort d n) d n

(+^|), (+.|) :: 
    (RoundedMixedAdd t tn) => 
    t -> tn -> t
(+^|) d n = mixedAddUpEff (mixedAddDefaultEffort d n) d n
(+.|) d n = mixedAddDnEff (mixedAddDefaultEffort d n) d n

(|*^), (|*.) :: 
    (RoundedMixedMultiply t tn) => 
    tn -> t -> t
(|*^) n d = mixedMultUpEff (mixedMultDefaultEffort d n) d n
(|*.) n d = mixedMultDnEff (mixedMultDefaultEffort d n) d n

(*^|), (*.|) :: 
    (RoundedMixedMultiply t tn) => 
    t -> tn -> t
(*^|) d n = mixedMultUpEff (mixedMultDefaultEffort d n) d n
(*.|) d n = mixedMultDnEff (mixedMultDefaultEffort d n) d n

(/^|), (/.|) :: 
    (RoundedMixedDivide t tn) => 
    t -> tn -> t
(/^|) d n = mixedDivUpEff (mixedDivDefaultEffort d n) d n
(/.|) d n = mixedDivDnEff (mixedDivDefaultEffort d n) d n

piUp, piDn ::
    (RoundedSpecialConst t) => 
    t -> t
piUp sample = piUpEff (specialConstDefaultEffort sample)
piDn sample = piDnEff (specialConstDefaultEffort sample)

eUp, eDn ::
    (RoundedSpecialConst t) => 
    t -> t
eUp sample = eUpEff (specialConstDefaultEffort sample)
eDn sample = eDnEff (specialConstDefaultEffort sample)

expUp, expDn ::
    (RoundedExponentiation t) => 
    t -> t
expUp d = expUpEff (expDefaultEffort d) d
expDn d = expDnEff (expDefaultEffort d) d

sqrtUp, sqrtDn ::
    (RoundedSquareRoot t) => 
    t -> t
sqrtUp d = sqrtUpEff (sqrtDefaultEffort d) d
sqrtDn d = sqrtDnEff (sqrtDefaultEffort d) d

