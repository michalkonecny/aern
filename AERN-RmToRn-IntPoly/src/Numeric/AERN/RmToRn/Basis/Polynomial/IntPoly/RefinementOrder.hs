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
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.NumericOrder
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
--import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Interval

import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.Effort

import Numeric.AERN.Misc.Debug

import Test.QuickCheck

instance
    (Ord var, Show var, 
     Show cf, ArithInOut.RoundedReal cf, 
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

--
--instance
--    (Show var, Ord var,
--     Show cf, RefOrd.ArbitraryOrderedTuple cf, RefOrd.IntervalLike cf,
--     ArithInOut.RoundedReal cf,
--     ArithInOut.RoundedMixedMultiply cf cf,
--     ArithInOut.RoundedMixedAdd cf cf,
--     Show (Imprecision cf),
--     NumOrd.PartialComparison (Imprecision cf),
--     Arbitrary (RefOrd.GetEndpointsEffortIndicator cf),
--     Arbitrary (ArithInOut.MixedMultEffortIndicator cf cf),
--     Arbitrary (ArithInOut.MixedAddEffortIndicator cf cf),
--     Arbitrary (ArithInOut.RoundedRealEffortIndicator cf),
--     Arbitrary (ArithInOut.AddEffortIndicator cf)
--    )
--    =>
--    RefOrd.ArbitraryOrderedTuple (IntPoly var cf)
--    where
--    type RefOrd.Area (IntPoly var cf) = Area4FunFromRingOps (IntPoly var cf)
--    areaWhole sampleF = areaWhole4FunFromRingOps sampleF
--    arbitraryTupleRelatedBy = 
--        error "AERN internal error: arbitraryTupleRelatedBy not defined for IntPoly"
--    arbitraryTupleInAreaRelatedBy area@(sampleFn,_) indices rels =
--        case 
--            arbitraryTupleInAreaRelatedBy4FunFromRingOps 
--                dummyEff 
--                (fnSequence fixedRandSeqQuantityOfSize sampleFn) 
--                fixedRandSeqQuantityOfSize 
--                area indices rels of
--            Nothing -> Nothing
--            Just _ -> Just $
--                do
--                eff <- arbitrary
--                case
--                    arbitraryTupleInAreaRelatedBy4FunFromRingOps 
--                        eff 
--                        (fnSequence fixedRandSeqQuantityOfSize sampleFn) 
--                        fixedRandSeqQuantityOfSize 
--                        area indices rels of
--                    Just gen -> gen
--        where
--        fixedRandSeqQuantityOfSize :: Int -> Int
--        fixedRandSeqQuantityOfSize size
--            = 10 + ((size*(size+100)) `div` 10)  
----            = 10 + (3*size)
--        dummyEff = 
--            ((e,e,e),
--             (e,e),
--             (e,e)
--            )
--        e :: t
--        e = error "AERN internal error: arbitraryTupleInAreaRelatedBy4FunFromRingOps: dummyEff should not be used"
--        