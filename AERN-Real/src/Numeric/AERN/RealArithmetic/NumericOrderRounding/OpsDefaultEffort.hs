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
infixr 7 |*., |*^
infixl 7 |/., |/^

(+^), (+.) :: 
    (RoundedAdd t) => 
    t -> t -> t
(+^) a = addUpEff (addDefaultEffort a) a
(+.) a = addDnEff (addDefaultEffort a) a

(-^), (-.) :: 
    (RoundedSubtr t) => 
    t -> t -> t
(-^) a = subtrUpEff (addDefaultEffort a) a
(-.) a = subtrDnEff (addDefaultEffort a) a

(*^), (*.) :: 
    (RoundedMultiply t) => 
    t -> t -> t
(*^) a = multUpEff (multDefaultEffort a) a
(*.) a = multDnEff (multDefaultEffort a) a

(^^), (^.) :: 
    (RoundedPowerToNonnegInt t) => 
    t -> Int -> t
(^^) a = powerToNonnegIntUpEff (powerToNonnegIntDefaultEffort a) a
(^.) a = powerToNonnegIntDnEff (powerToNonnegIntDefaultEffort a) a

(/^), (/.) :: 
    (RoundedDivide t) => 
    t -> t -> t
(/^) a = divUpEff (divDefaultEffort a) a
(/.) a = divDnEff (divDefaultEffort a) a

(|+^), (|+.) :: 
    (RoundedMixedAdd t1 t2) => 
    t1 -> t2 -> t2
(|+^) a b = mixedAddUpEff (mixedAddDefaultEffort a b) a b
(|+.) a b = mixedAddDnEff (mixedAddDefaultEffort a b) a b

(|*^), (|*.) :: 
    (RoundedMixedMultiply t1 t2) => 
    t1 -> t2 -> t2
(|*^) a b = mixedMultUpEff (mixedMultDefaultEffort a b) a b
(|*.) a b = mixedMultDnEff (mixedMultDefaultEffort a b) a b

(|/^), (|/.) :: 
    (RoundedMixedDivide t1 t2) => 
    t2 -> t1 -> t2
(|/^) a b = mixedDivUpEff (mixedDivDefaultEffort b a) a b
(|/.) a b = mixedDivDnEff (mixedDivDefaultEffort b a) a b

