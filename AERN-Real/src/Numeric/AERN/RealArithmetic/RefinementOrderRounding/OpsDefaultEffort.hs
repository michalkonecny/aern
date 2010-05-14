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

