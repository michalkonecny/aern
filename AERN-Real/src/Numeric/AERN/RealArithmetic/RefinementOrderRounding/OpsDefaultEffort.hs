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
infixl 8 <^>, >^<
infixl 7 </>, >/<

infixr 6 |<+>, |>+<
infixl 6 <+>|, >+<|
infixr 7 |<*>, |>*<
infixl 7 <*>|, >*<|
infixl 7 </>|, >/<|

(>+<), (<+>) :: 
    (RoundedAdd t) => 
    t -> t -> t
(>+<) d = addInEff (addDefaultEffort d) d
(<+>) d = addOutEff (addDefaultEffort d) d

(>-<), (<->) :: 
    (RoundedSubtr t) => 
    t -> t -> t
(>-<) d = subtrInEff (addDefaultEffort d) d
(<->) d = subtrOutEff (addDefaultEffort d) d

(>*<), (<*>) :: 
    (RoundedMultiply t) => 
    t -> t -> t
(>*<) d = multInEff (multDefaultEffort d) d
(<*>) d = multOutEff (multDefaultEffort d) d

(>^<), (<^>) :: 
    (RoundedPowerToNonnegInt t) => 
    t -> Int -> t
(>^<) d = powerToNonnegIntInEff (powerToNonnegIntDefaultEffort d) d
(<^>) d = powerToNonnegIntOutEff (powerToNonnegIntDefaultEffort d) d

(>/<), (</>) :: 
    (RoundedDivide t) => 
    t -> t -> t
(>/<) d = divInEff (divDefaultEffort d) d
(</>) d = divOutEff (divDefaultEffort d) d


(|>+<), (|<+>) :: 
    (RoundedMixedAdd t tn) => 
    tn -> t -> t
(|>+<) n d = mixedAddInEff (mixedAddDefaultEffort d n) d n
(|<+>) n d = mixedAddOutEff (mixedAddDefaultEffort d n) d n

(>+<|), (<+>|) :: 
    (RoundedMixedAdd t tn) => 
    t -> tn -> t
(>+<|) d n = mixedAddInEff (mixedAddDefaultEffort d n) d n
(<+>|) d n = mixedAddOutEff (mixedAddDefaultEffort d n) d n

(|>*<), (|<*>) :: 
    (RoundedMixedMultiply t tn) => 
    tn -> t -> t
(|>*<) n d = mixedMultInEff (mixedMultDefaultEffort d n) d n
(|<*>) n d = mixedMultOutEff (mixedMultDefaultEffort d n) d n

(>*<|), (<*>|) :: 
    (RoundedMixedMultiply t tn) => 
    t -> tn -> t
(>*<|) d n = mixedMultInEff (mixedMultDefaultEffort d n) d n
(<*>|) d n = mixedMultOutEff (mixedMultDefaultEffort d n) d n


(>/<|), (</>|) :: 
    (RoundedMixedDivide t tn) => 
    t -> tn -> t
(>/<|) d n = mixedDivInEff (mixedDivDefaultEffort d n) d n
(</>|) d n = mixedDivOutEff (mixedDivDefaultEffort d n) d n
