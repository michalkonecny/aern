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

(*^), (*.) :: 
    (RoundedMultiply t, ?multUpDnEffort :: MultEffortIndicator t) => 
    t -> t -> t
(*^) = multUpEff ?multUpDnEffort
(*.) = multDnEff ?multUpDnEffort

(/^), (/.) :: 
    (RoundedDivide t, ?divUpDnEffort :: DivEffortIndicator t) => 
    t -> t -> t
(/^) = divUpEff ?divUpDnEffort
(/.) = divDnEff ?divUpDnEffort
