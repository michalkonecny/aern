{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.Poly.IntPoly.Minmax
    Description :  pointwise min/max
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Pointwise min/max of interval polynomials rounding up/down/in/out.
-}

module Numeric.AERN.Poly.IntPoly.Minmax
(
    minmaxUpDnDefaultEffortIntPolyWithBezierDegree,
    minmaxInOutDefaultEffortIntPolyWithBezierDegree
)
where
    
import Numeric.AERN.Poly.IntPoly.Config
import Numeric.AERN.Poly.IntPoly.IntPoly
import Numeric.AERN.Poly.IntPoly.New
import Numeric.AERN.Poly.IntPoly.Evaluation
import Numeric.AERN.Poly.IntPoly.NumericOrder
import Numeric.AERN.Poly.IntPoly.RefinementOrder
import Numeric.AERN.Poly.IntPoly.Addition
import Numeric.AERN.Poly.IntPoly.Multiplication
import Numeric.AERN.Poly.IntPoly.Composition

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation

--import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Comparison
--import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Arbitrary
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

import Test.QuickCheck (Arbitrary)

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
        (MinmaxEffortIndicatorFromRingOps (IntPoly var cf) (IntPoly var cf),
         Int1To10, -- ^ (degree of Bernstein approximations) - 1   (the degree must be > 1)
         RefOrd.GetEndpointsEffortIndicator (IntPoly var cf))
    minmaxDefaultEffort f =
        (defaultMinmaxEffortIndicatorFromRingOps f f,
         Int1To10 3, -- degree 4
         RefOrd.getEndpointsDefaultEffort f)

minmaxUpDnDefaultEffortIntPolyWithBezierDegree degree f =
    (defaultMinmaxEffortIndicatorFromRingOps f f,
     Int1To10 (degree - 1),
     RefOrd.getEndpointsDefaultEffort f)

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
    maxUpEff (effMinmax,Int1To10 degreeMinusOne,effGetE) a b =
--        unsafePrint 
--            ( "IntPoly maxUpEff:"
--                ++ "\n a = " ++ showPoly show show a
--                ++ "\n b = " ++ showPoly show show b
--                ++ "\n a `maxUp` b = " ++ showPoly show show result 
--            ) $
        result
        where
        (result, _) = maxUpEffFromRingOps a getX effMinmax (getDegree degreeMinusOne a) aR bR
        (_aL,aR) = RefOrd.getEndpointsOutEff effGetE a
        (_bL,bR) = RefOrd.getEndpointsOutEff effGetE b
    maxDnEff (effMinmax,Int1To10 degreeMinusOne,effGetE) a b =
        result
        where
        result = maxDnEffFromRingOps a getX effMinmax (getDegree degreeMinusOne a) aL bL
        (aL,_aR) = RefOrd.getEndpointsOutEff effGetE a
        (bL,_bR) = RefOrd.getEndpointsOutEff effGetE b
    minUpEff (effMinmax,Int1To10 degreeMinusOne,effGetE) a b = 
        result
        where
        result = minUpEffFromRingOps a getX effMinmax (getDegree degreeMinusOne a) aR bR
        (_aL,aR) = RefOrd.getEndpointsOutEff effGetE a
        (_bL,bR) = RefOrd.getEndpointsOutEff effGetE b
    minDnEff (effMinmax,Int1To10 degreeMinusOne,effGetE) a b = 
        result
        where
        (result, _) = minDnEffFromRingOps a getX effMinmax (getDegree degreeMinusOne a) aL bL
        (aL,_aR) = RefOrd.getEndpointsOutEff effGetE a
        (bL,_bR) = RefOrd.getEndpointsOutEff effGetE b

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
    NumOrd.RefinementRoundedLatticeEffort (IntPoly var cf)
    where
    type NumOrd.MinmaxInOutEffortIndicator (IntPoly var cf) =
        (MinmaxEffortIndicatorFromRingOps (IntPoly var cf) (IntPoly var cf),
         Int1To10, -- ^ (degree of Bernstein approximations) - 1   (the degree must be > 1)
         RefOrd.GetEndpointsEffortIndicator (IntPoly var cf),
         RefOrd.FromEndpointsEffortIndicator (IntPoly var cf))

    minmaxInOutDefaultEffort f =
        (defaultMinmaxEffortIndicatorFromRingOps f f, 
         Int1To10 3, -- degree 4
         RefOrd.getEndpointsDefaultEffort f,
         RefOrd.fromEndpointsDefaultEffort f)

minmaxInOutDefaultEffortIntPolyWithBezierDegree degree f =
    (defaultMinmaxEffortIndicatorFromRingOps f f,
     Int1To10 (degree - 1),
     RefOrd.getEndpointsDefaultEffort f,
     RefOrd.fromEndpointsDefaultEffort f)

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
    NumOrd.RefinementRoundedLattice (IntPoly var cf)
    where
    maxOutEff (effMinmax,Int1To10 degreeMinusOne,effGetE,effFromE) a b =
        result
        where
        result = RefOrd.fromEndpointsOutEff effFromE (resL,resR)
        (resR, resL) = maxUpEffFromRingOps a getX effMinmax (getDegree degreeMinusOne a) aR bR
        (aL,aR) = RefOrd.getEndpointsOutEff effGetE a
        (bL,bR) = RefOrd.getEndpointsOutEff effGetE b
    maxInEff =
        error "aern-poly: inner-rounded max not available for IntPoly"
    minOutEff (effMinmax,Int1To10 degreeMinusOne,effGetE,effFromE) a b =
        result
        where
        result = RefOrd.fromEndpointsOutEff effFromE (resL,resR)
        (resL, resR) = minDnEffFromRingOps a getX effMinmax (getDegree degreeMinusOne a) aL bL
        (aL,aR) = RefOrd.getEndpointsOutEff effGetE a
        (bL,bR) = RefOrd.getEndpointsOutEff effGetE b
    minInEff =
        error "aern-poly: inner-rounded min not available for IntPoly"
    
    
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
    RefOrd.RoundedLatticeEffort (IntPoly var cf)
    where
    type RefOrd.JoinMeetEffortIndicator (IntPoly var cf) =
        RefOrd.JoinMeetEffortIndicator cf

    joinmeetDefaultEffort (IntPoly cfg _) =
        RefOrd.joinmeetDefaultEffort $ ipolycfg_sample_cf cfg

--joinmeetDefaultEffortIntPolyWithBezierDegree degree f =
--    (defaultMinmaxEffortIndicatorFromRingOps f f,
--     Int1To10 (degree - 1),
--     RefOrd.getEndpointsDefaultEffort f,
--     RefOrd.fromEndpointsDefaultEffort f)

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
    RefOrd.RoundedLattice (IntPoly var cf)
    where
    joinOutEff =
        error "aern-poly: join not defined for IntPoly"
    joinInEff =
        error "aern-poly: join not defined for IntPoly"
    meetOutEff eff a b@(IntPoly cfg _) =
        polyJoinWith (zero sampleCf) (uncurry $ RefOrd.meetOutEff eff) (a, b)
        where
        sampleCf = ipolycfg_sample_cf cfg
    meetInEff =
        error "aern-poly: inner-rounded meet not defined for IntPoly"

getX sizeLimits@(IntPolyCfg vars _ _ sample md ms) =
    newProjection cfg dombox var
    where
    _ = [sizeLimits, cfg] -- , getSizeLimits sampleT]
    var = head vars
    cfg =
        IntPolyCfg [var] [unit] [zero sample] sample md ms
    dombox = fromList $ cfg2vardomains cfg
    unit =
        RefOrd.fromEndpointsOutWithDefaultEffort (zero sample, one sample)
    
getDegree degreeMinusOne (IntPoly cfg _) =
    max 2 $ min (degreeMinusOne + 1) maxDeg
    where
    maxDeg = ipolycfg_maxdeg cfg