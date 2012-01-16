{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Minmax
    Description :  pointwise min/max
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Pointwise min/max of interval polynomials rounding up/down/in/out.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Minmax
(
    minmaxDefaultEffortIntPolyWithBezierDegree
)
where
    
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Evaluation
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.NumericOrder
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Substitution

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain
--import Numeric.AERN.RmToRn.Evaluation

import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Comparison
import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Arbitrary
import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Minmax

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Consistency

import Numeric.AERN.Misc.Debug

import Test.QuickCheck

instance
    (Ord var,
     GeneratableVariables var,
     HasAntiConsistency cf, 
     Arbitrary cf, 
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,
     ArithInOut.RoundedMixedField cf cf,
     Show var,
     Show cf,
     Show (Imprecision cf),
     NumOrd.PartialComparison (Imprecision cf))
    =>
    NumOrd.RoundedLatticeEffort (IntPoly var cf)
    where
    type NumOrd.MinmaxEffortIndicator (IntPoly var cf) =
        MinmaxInOutEffortIndicatorFromRingOps (IntPoly var cf) (IntPoly var cf)
    minmaxDefaultEffort f =
        defaultMinmaxInOutEffortIndicatorFromRingOps f f 

minmaxDefaultEffortIntPolyWithBezierDegree degree f =
    defaultMinmaxInOutEffortIndicatorFromRingOpsDegree degree f f

instance
    (Ord var,
     GeneratableVariables var,
     HasAntiConsistency cf, 
     Arbitrary cf, 
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,
     ArithInOut.RoundedMixedField cf cf,
     Show var,
     Show cf,
     Show (Imprecision cf),
     NumOrd.PartialComparison (Imprecision cf))
    =>
    NumOrd.RoundedLattice (IntPoly var cf)
    where
    maxUpEff eff a b =
--        unsafePrint 
--            ( "IntPoly maxUpEff:"
--                ++ "\n a = " ++ showPoly show show a
--                ++ "\n b = " ++ showPoly show show b
--                ++ "\n a `maxUp` b = " ++ showPoly show show result 
--            ) $
        result
        where
        result = maxUpEffFromRingOps a getX eff a b
    maxDnEff eff a b =
        result
        where
        result =  
            maxDnEffFromRingOps a getX eff a b
    minUpEff eff a b = 
        result
        where
        result =  
            neg $ maxDnEffFromRingOps a getX eff (neg a) (neg b)
    minDnEff eff a b = 
--        unsafePrint 
--            ( "IntPoly minDnEff:"
--                ++ "\n a = " ++ showPoly show show a
--                ++ "\n b = " ++ showPoly show show b
--                ++ "\n a `minDn` b = " ++ showPoly show show result 
--            ) $
        result
        where
        result = neg $ maxUpEffFromRingOps a getX eff (neg a) (neg b)
    
getX sizeLimits@(IntPolyCfg vars doms sample md ms) =
    newProjection cfg undefined var
    where
    _ = [sizeLimits, cfg] -- , getSizeLimits sampleT]
    var = head vars
    cfg =
        IntPolyCfg [var] [unit] sample md ms
    unit =
        RefOrd.fromEndpointsOutWithDefaultEffort (zero sample, one sample)
    