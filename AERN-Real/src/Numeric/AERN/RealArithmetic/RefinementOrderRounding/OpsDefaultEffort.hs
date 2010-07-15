{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
    Description :  convenience binary infix operators with default effort  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience binary infix operators with default effort.
-}

module Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort where

import Numeric.AERN.RealArithmetic.RefinementOrderRounding

infixl 6 <+>, >+<, <->, >-<
infixl 7 <*>, >*<
infixl 7 </>, >/<

infixr 6 |<+>, |>+<
infixr 7 |<*>, |>*<
infixl 7 |</>, |>/<

(>+<), (<+>) :: 
    (RoundedAdd t) => 
    t -> t -> t
(>+<) a = addInEff (addDefaultEffort a) a
(<+>) a = addOutEff (addDefaultEffort a) a

(>-<), (<->) :: 
    (RoundedSubtr t) => 
    t -> t -> t
(>-<) a = subtrInEff (addDefaultEffort a) a
(<->) a = subtrOutEff (addDefaultEffort a) a

(>*<), (<*>) :: 
    (RoundedMultiply t) => 
    t -> t -> t
(>*<) a = multInEff (multDefaultEffort a) a
(<*>) a = multOutEff (multDefaultEffort a) a

(>/<), (</>) :: 
    (RoundedDivide t) => 
    t -> t -> t
(>/<) a = divInEff (divDefaultEffort a) a
(</>) a = divOutEff (divDefaultEffort a) a


(|>+<), (|<+>) :: 
    (RoundedMixedAdd t1 t2) => 
    t1 -> t2 -> t2
(|>+<) a b = mixedAddInEff (mixedAddDefaultEffort a b) a b
(|<+>) a b = mixedAddOutEff (mixedAddDefaultEffort a b) a b

(|>*<), (|<*>) :: 
    (RoundedMixedMultiply t1 t2) => 
    t1 -> t2 -> t2
(|>*<) a b = mixedMultInEff (mixedMultDefaultEffort a b) a b
(|<*>) a b = mixedMultOutEff (mixedMultDefaultEffort a b) a b


(|>/<), (|</>) :: 
    (RoundedMixedDivide t1 t2) => 
    t2 -> t1 -> t2
(|>/<) a b = mixedDivInEff (mixedDivDefaultEffort b a) a b
(|</>) a b = mixedDivOutEff (mixedDivDefaultEffort b a) a b
