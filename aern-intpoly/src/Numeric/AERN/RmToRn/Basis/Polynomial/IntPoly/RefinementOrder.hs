{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RefinementOrder
    Description :  pointwise in/out comparison
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Pointwise in/out comparison of interval polynomials. 
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RefinementOrder
where
    
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics
--import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.NumericOrder.Comparison
--import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Evaluation

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain
--import Numeric.AERN.RmToRn.Evaluation

--import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Comparison
--import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Arbitrary
--import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Minmax

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures


import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Interval

import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Consistency

import Numeric.AERN.Misc.Debug

import Test.QuickCheck

instance
    (RefOrd.IntervalLike cf, HasZero cf)
    => 
    (RefOrd.IntervalLike (IntPoly var cf))
    where
    type RefOrd.GetEndpointsEffortIndicator (IntPoly var cf) = 
        RefOrd.GetEndpointsEffortIndicator cf
    type RefOrd.FromEndpointsEffortIndicator (IntPoly var cf) = 
        RefOrd.FromEndpointsEffortIndicator cf
    getEndpointsDefaultEffort (IntPoly cfg _) =
        RefOrd.getEndpointsDefaultEffort sampleCf
        where
        sampleCf = ipolycfg_sample_cf cfg
    fromEndpointsDefaultEffort (IntPoly cfg _) =
        RefOrd.fromEndpointsDefaultEffort sampleCf
        where
        sampleCf = ipolycfg_sample_cf cfg
    getEndpointsInEff eff = polySplitWith (RefOrd.getEndpointsInEff eff)
    getEndpointsOutEff eff = polySplitWith (RefOrd.getEndpointsOutEff eff)
    fromEndpointsInEff eff pp@(IntPoly cfg _, _) = 
        polyJoinWith z (RefOrd.fromEndpointsInEff eff) pp
        where 
        z = zero $ ipolycfg_sample_cf cfg 
    fromEndpointsOutEff eff pp@(IntPoly cfg _, _) = 
        polyJoinWith z (RefOrd.fromEndpointsOutEff eff) pp
        where 
        z = zero $ ipolycfg_sample_cf cfg 
    getEndpointsInWithDefaultEffort = polySplitWith (RefOrd.getEndpointsInWithDefaultEffort)
    getEndpointsOutWithDefaultEffort = polySplitWith (RefOrd.getEndpointsOutWithDefaultEffort)
    fromEndpointsInWithDefaultEffort pp@(IntPoly cfg _, _) = 
        polyJoinWith z (RefOrd.fromEndpointsInWithDefaultEffort) pp
        where 
        z = zero $ ipolycfg_sample_cf cfg 
    fromEndpointsOutWithDefaultEffort pp@(IntPoly cfg _, _) = 
        polyJoinWith z (RefOrd.fromEndpointsOutWithDefaultEffort) pp
        where 
        z = zero $ ipolycfg_sample_cf cfg 

instance
    (Ord var, Show var, 
     Show cf, ArithInOut.RoundedReal cf, 
     HasAntiConsistency cf,
     NumOrd.PartialComparison (Imprecision cf), 
     RefOrd.IntervalLike cf) 
    => 
    RefOrd.PartialComparison (IntPoly var cf) 
    where
    type RefOrd.PartialCompareEffortIndicator (IntPoly var cf) =
        (NumOrd.PartialCompareEffortIndicator (IntPoly var cf), 
         RefOrd.GetEndpointsEffortIndicator (IntPoly var cf))
    pCompareDefaultEffort fn = 
        (NumOrd.pCompareDefaultEffort fn,
         RefOrd.getEndpointsDefaultEffort fn)
    pCompareEff effort p1 p2 =
        case partialInfo2PartialOrdering $ RefOrd.pCompareInFullEff effort p1 p2 of
            [ord] -> Just ord
            _ -> Nothing
    pCompareInFullEff (effNumComp, effGetE) p1 p2 =
        RefOrd.pCompareInFullEff effNumComp (Interval l1 r1) (Interval l2 r2)
        where
        (l1, r1) = RefOrd.getEndpointsOutEff effGetE p1 
        (l2, r2) = RefOrd.getEndpointsOutEff effGetE p2
        
instance
    (Ord var,
     RefOrd.HasTop cf,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf
    )
    =>
    RefOrd.HasTop (IntPoly var cf)
    where
    top sampleF = 
        newConstFnFromSample sampleF (RefOrd.top sampleDom)
        where
        sampleDom = getSampleDomValue sampleF
        
instance
    (Ord var,
     RefOrd.HasBottom cf,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf
    )
    =>
    RefOrd.HasBottom (IntPoly var cf)
    where
    bottom sampleF = 
        newConstFnFromSample sampleF (RefOrd.bottom sampleDom)
        where
        sampleDom = getSampleDomValue sampleF
        
instance
    (Ord var,
     RefOrd.HasExtrema cf,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf
    )
    =>
    RefOrd.HasExtrema (IntPoly var cf)
        
        
instance 
    (NumOrd.ArbitraryOrderedTuple (IntPoly var cf), RefOrd.IntervalLike cf, HasZero cf) 
    => 
    RefOrd.ArbitraryOrderedTuple (IntPoly var cf) where
    type RefOrd.Area (IntPoly var cf) = NumOrd.Area (IntPoly var cf)
    areaWhole p = NumOrd.areaWhole p
    arbitraryTupleRelatedBy = 
        error "AERN internal error: RefOrd.arbitraryTupleRelatedBy not defined for IntPoly"
    arbitraryTupleInAreaRelatedBy area indices rels =
        case RefOrd.arbitraryTupleInAreaRelatedBy area indices rels of
            Nothing -> Nothing
            Just gen ->
                Just $
                do
                intervals <- gen
                effFromE <- arbitrary
                return $ map (unInterval effFromE) intervals
                where
                unInterval effFromE (Interval l r) =
                    RefOrd.fromEndpointsOutEff effFromE (l, r)

instance
    (Ord var, Show var, 
     Show cf, ArithInOut.RoundedReal cf,
     HasAntiConsistency cf,
     NumOrd.PartialComparison (Imprecision cf), 
     RefOrd.IntervalLike cf) 
    => 
    HasConsistency (IntPoly var cf)
    where
    type ConsistencyEffortIndicator (IntPoly var cf) =
        (NumOrd.PartialCompareEffortIndicator (IntPoly var cf), 
         RefOrd.GetEndpointsEffortIndicator (IntPoly var cf))
    consistencyDefaultEffort p =
        (NumOrd.pCompareDefaultEffort p,
         RefOrd.getEndpointsDefaultEffort p)
    getConsistencyEff (effComp, effGetE) p =
        getConsistencyEff effComp (Interval l r)
        where
        (l, r) = RefOrd.getEndpointsOutEff effGetE p 

instance
    (Ord var, Show var, 
     Show cf, ArithInOut.RoundedReal cf,
     HasAntiConsistency cf, 
     NumOrd.PartialComparison (Imprecision cf), 
     RefOrd.IntervalLike cf) 
    => 
    HasAntiConsistency (IntPoly var cf)
    where
    flipConsistency = flipConsistencyPoly
        
    