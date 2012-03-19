{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.Poly.IntPoly.RefinementOrder
    Description :  interval polynomial as a polynomial interval
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Interval polynomial as a polynomial interval.
-}

module Numeric.AERN.Poly.IntPoly.RefinementOrder
where
    
import Numeric.AERN.Poly.IntPoly.Config
import Numeric.AERN.Poly.IntPoly.IntPoly
import Numeric.AERN.Poly.IntPoly.Evaluation ()
--import Numeric.AERN.Poly.IntPoly.Addition 
import Numeric.AERN.Poly.IntPoly.Multiplication ()
import Numeric.AERN.Poly.IntPoly.NumericOrder ()

--import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain
--import Numeric.AERN.RmToRn.Evaluation

--import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Comparison
--import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Arbitrary
--import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Minmax

--import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Interval (refordPCompareInFullIntervalsEff)

import Numeric.AERN.Basics.PartialOrdering
--import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Consistency

--import Numeric.AERN.Misc.Debug

instance
    (Ord var, Show var, 
     Show cf, 
     ArithInOut.RoundedReal cf,
     HasAntiConsistency cf,
     NumOrd.PartialComparison (Imprecision cf), 
     RefOrd.IntervalLike cf) 
    => 
    RefOrd.PartialComparison (IntPoly var cf) 
    where
    type RefOrd.PartialCompareEffortIndicator (IntPoly var cf) =
        (NumOrd.PartialCompareEffortIndicator (IntPoly var cf),
         RefOrd.GetEndpointsEffortIndicator cf) 
    pCompareDefaultEffort p =
        (NumOrd.pCompareDefaultEffort p, RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue p 
    pCompareEff eff p1 p2 =
        case partialInfo2PartialOrdering $ RefOrd.pCompareInFullEff eff p1 p2 of
            [rel] -> Just rel
            _ -> Nothing
    pCompareInFullEff (effNumComp, effGetE) p1 p2 = 
        refordPCompareInFullIntervalsEff effNumComp p1Endpoints p2Endpoints  
        where
        p1Endpoints = polyGetEndpointsOutEff effGetE p1
        p2Endpoints = polyGetEndpointsOutEff effGetE p2
        
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
    (RefOrd.IntervalLike cf, HasZero cf, 
     NumOrd.PartialComparison (IntPoly var cf)
    )
    => 
    (HasConsistency (IntPoly var cf))
    where
    type ConsistencyEffortIndicator (IntPoly var cf) =
        (RefOrd.GetEndpointsEffortIndicator cf,
         RefOrd.FromEndpointsEffortIndicator cf,
         NumOrd.PartialCompareEffortIndicator (IntPoly var cf))
    -- TODO
    
instance 
    (HasAntiConsistency cf,
     RefOrd.IntervalLike cf, HasZero cf, 
     NumOrd.PartialComparison (IntPoly var cf)
    )
    => 
    (HasAntiConsistency (IntPoly var cf))
    where
    flipConsistency = flipConsistencyPoly
    