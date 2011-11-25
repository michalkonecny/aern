{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.NumericOrder.FromInOutRingOps.RefinementRoundedLattice
    Description :  approximation of min and max using only ring operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Approximation of min and max using only ring operations.
-}

module Numeric.AERN.NumericOrder.FromInOutRingOps.RefinementRoundedLattice where

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.OpsImplicitEffort

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsImplicitEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort
import Numeric.AERN.RefinementOrder.InPlace.OpsImplicitEffort

import qualified Numeric.AERN.NumericOrder as NumOrd

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Mutable
import Numeric.AERN.RealArithmetic.ExactOps

import Control.Monad.ST (ST)

type MinmaxInOutEffortIndicatorFromRingOps t =
    (Int,
     ArithInOut.AddEffortIndicator t,
     ArithInOut.MultEffortIndicator t)

maxOutEffFromRingOps :: 
    (ArithInOut.RoundedAdd t, 
     ArithInOut.RoundedSubtr t, 
     ArithInOut.RoundedMultiply t, 
     ArithInOut.RoundedPowerToNonnegInt t) =>
    MinmaxInOutEffortIndicatorFromRingOps t -> t -> t -> t
maxOutEffFromRingOps eff@(_, effAdd, _) a b =
    let ?addInOutEffort = effAdd in
    a <+> (maxZero eff $ b <-> a)

maxZero ::    
    (ArithInOut.RoundedAdd t, 
     ArithInOut.RoundedSubtr t, 
     ArithInOut.RoundedMultiply t, 
     ArithInOut.RoundedPowerToNonnegInt t) =>
    MinmaxInOutEffortIndicatorFromRingOps t -> t -> t
maxZero (degree, effAdd, effMult) x =
    error $ "maxZero not implemented yet"
    
{-
  The motivating use case for this function is where x is a function
  and we need to compute maxZero pointwise over the whole domain of x.
  
    * find bounds for x 
      (what type should the bound be? 
       Is Double accurate enough or do we need to introduce another type parameter?) 
    * if x can be shown to be positive or negative, finish
    * affinely transform x to x' so that it fits inside [0,1] 
      and note the point 0 < c < 1 where the original crossed zero
    * compute a Bernstein approximation of the given degree to the function \y -> \max(0,y-c)
    * compute a reliable estimate e of the approximation error
    * r' = evaluate this polynomial with x' for y and add [-e,0] 
    * transform r' back to the range of x to get the result r 
-}    
    
    

    