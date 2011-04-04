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

-- | inner rounded addition
(>+<) :: 
    (RoundedAdd t) => 
    t -> t -> t
(>+<) d = addInEff (addDefaultEffort d) d

-- | outer rounded addition
(<+>) :: 
    (RoundedAdd t) => 
    t -> t -> t
(<+>) d = addOutEff (addDefaultEffort d) d

(>-<), (<->) :: 
    (RoundedSubtr t) => 
    t -> t -> t
(>-<) d = subtrInEff (addDefaultEffort d) d
(<->) d = subtrOutEff (addDefaultEffort d) d

absIn, absOut ::
    (RoundedAbs t) => 
    t -> t
absIn d = absInEff (absDefaultEffort d) d
absOut d = absOutEff (absDefaultEffort d) d

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


piIn, piOut ::
    (RoundedSpecialConst t) => 
    t
piIn = result
    where
    result =  
        piInEff (specialConstDefaultEffort result)
piOut = result
    where
    result =  
        piOutEff (specialConstDefaultEffort result)

eIn, eOut ::
    (RoundedSpecialConst t) => 
    t
eIn = result
    where
    result =  
        eInEff (specialConstDefaultEffort result)
eOut = result
    where
    result =  
        eOutEff (specialConstDefaultEffort result)


expIn, expOut ::
    (RoundedExponentiation t) => 
    t -> t
expIn d = expInEff (expDefaultEffort d) d
expOut d = expOutEff (expDefaultEffort d) d

sqrtIn, sqrtOut ::
    (RoundedSquareRoot t) => 
    t -> t
sqrtIn d = sqrtInEff (sqrtDefaultEffort d) d
sqrtOut d = sqrtOutEff (sqrtDefaultEffort d) d

