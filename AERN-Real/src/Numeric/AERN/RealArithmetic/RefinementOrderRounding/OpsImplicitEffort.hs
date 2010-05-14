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

(>*<), (<*>) :: 
    (RoundedMultiply t, ?multInOutEffort :: MultEffortIndicator t) => 
    t -> t -> t
(>*<) = multInEff ?multInOutEffort
(<*>) = multOutEff ?multInOutEffort

(>/<), (</>) :: 
    (RoundedDivide t, ?divInOutEffort :: DivEffortIndicator t) => 
    t -> t -> t
(>/<) = divInEff ?divInOutEffort
(</>) = divOutEff ?divInOutEffort

