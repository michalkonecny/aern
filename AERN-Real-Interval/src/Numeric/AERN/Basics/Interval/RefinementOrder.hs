{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.Basics.Interval.RefinementOrder
    Description :  interval instances of refinement-ordered structures 
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Interval instances of refinement-ordered structures.
    
    This is a hidden module reexported via its parent.
-}
module Numeric.AERN.Basics.Interval.RefinementOrder where

import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.Basics.Effort 
import Numeric.AERN.Basics.PartialOrdering

import Numeric.AERN.Basics.Interval.Basics
import Numeric.AERN.Basics.Interval.Mutable
import Numeric.AERN.Basics.Interval.NumericOrder

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd

import Numeric.AERN.Basics.Mutable

import Numeric.AERN.Misc.List

import Test.QuickCheck

import Data.Maybe

instance
    (RefOrd.IntervalLike (Interval e))
    where
    type RefOrd.GetEndpointsEffortIndicator (Interval e) = ()
    type RefOrd.FromEndpointsEffortIndicator (Interval e) = ()
    getEndpointsDefaultEffort _ = ()
    fromEndpointsDefaultEffort _ = ()
    getEndpointsInEff _ (Interval l r) = (Interval l l,Interval r r)
    getEndpointsOutEff _ (Interval l r) = (Interval l l,Interval r r)
    fromEndpointsInEff _ (Interval ll lr, Interval rl rr) = (Interval ll rr) 
        -- why not (Interval lr rl) ? because:
        --   we do not necessarily interpret intervals as approximations of singletons
        --   we are approximating an interval operation - which one?
        --      a suitable inverse of getEndpointsInEff, which produces anti-consistent endpoints
        --         inner rounded (from . get) must produce a sub-interval
        --           Interval lr rl here would break this property
        --   in this case no rounding occurs - Out and In versions must be the same
    fromEndpointsOutEff _ (Interval ll lr, Interval rl rr) = (Interval ll rr)
    fromEndpointsInWithDefaultEffort (Interval ll lr, Interval rl rr) = (Interval ll rr) 
    fromEndpointsOutWithDefaultEffort (Interval ll lr, Interval rl rr) = (Interval ll rr) 

instance 
    (NumOrd.PartialComparison e) => 
    (RefOrd.PartialComparison (Interval e))
    where
    type RefOrd.PartialCompareEffortIndicator (Interval e) = 
        NumOrd.PartialCompareEffortIndicator e 
    pCompareDefaultEffort (Interval l r) = 
        NumOrd.pCompareDefaultEffort l
    pCompareEff effort (Interval l1 r1) (Interval l2 r2) =
            case (c l1 l2, c r1 r2) of
                (Just EQ, Just EQ) -> Just EQ
                (Just LT, Just GT) -> Just LT  
                (Just LT, Just EQ) -> Just LT  
                (Just EQ, Just GT) -> Just LT  
                (Just GT, Just LT) -> Just GT  
                (Just GT, Just EQ) -> Just GT  
                (Just EQ, Just LT) -> Just GT  
                (Just _, Just _) -> Just NC  
                _ -> Nothing
            where
            c = NumOrd.pCompareEff effort 
        

instance (NumOrd.HasExtrema e) => (RefOrd.HasTop (Interval e))
    where
    top (Interval sampleE _) = 
        Interval (NumOrd.greatest sampleE) (NumOrd.least sampleE)
    
instance (NumOrd.HasExtrema e) => (RefOrd.HasBottom (Interval e))
    where
    bottom (Interval sampleE _) = 
        Interval (NumOrd.least sampleE) (NumOrd.greatest sampleE)

instance (NumOrd.HasExtrema e) => (RefOrd.HasExtrema (Interval e))

instance (NumOrd.RoundedLatticeEffort e, NumOrd.PartialComparison e) 
    => RefOrd.RoundedBasisEffort (Interval e)
    where
    type RefOrd.PartialJoinEffortIndicator (Interval e) = 
        (NumOrd.MinmaxEffortIndicator e, NumOrd.PartialCompareEffortIndicator e) 
    partialJoinDefaultEffort (Interval l r) =
        (NumOrd.minmaxDefaultEffort l, NumOrd.pCompareDefaultEffort l)
    
instance 
    (NumOrd.RoundedLattice e, NumOrd.PartialComparison e) => 
    RefOrd.RoundedBasis (Interval e) 
    where
    partialJoinOutEff (effortMinmax, effortComp) (Interval l1 r1) (Interval l2 r2) = 
            case l <=? r of
                Just True -> Just $ Interval l r
                _ -> Nothing
            where
            (<=?) = NumOrd.pLeqEff effortComp
            l = NumOrd.maxDnEff effortMinmax l1 l2
            r = NumOrd.minUpEff effortMinmax r1 r2
    partialJoinInEff (effortMinmax, effortComp) (Interval l1 r1) (Interval l2 r2) = 
            case l <=? r of
                Just True -> Just $ Interval l r
                _ -> Nothing
            where
            (<=?) = NumOrd.pLeqEff effortComp
            l = NumOrd.maxUpEff effortMinmax l1 l2
            r = NumOrd.minDnEff effortMinmax r1 r2

instance
    (NumOrd.RoundedLatticeInPlace e, NumOrd.PartialComparison e) =>
    (RefOrd.RoundedBasisInPlace (Interval e))
    where
    partialJoinOutInPlaceEff (effortMinmax, effortComp) 
            (MInterval resLM resRM) (MInterval l1M r1M) (MInterval l2M r2M) =
        do
        NumOrd.maxDnInPlaceEff effortMinmax resLM l1M l2M
        NumOrd.minUpInPlaceEff effortMinmax resRM r1M r2M
        l <- unsafeReadMutable resLM
        r <- unsafeReadMutable resRM
        let (<=?) = NumOrd.pLeqEff effortComp
        case l <=? r of
            Just True -> return True
            _ -> return False
    partialJoinInInPlaceEff (effortMinmax, effortComp) 
            (MInterval resLM resRM) (MInterval l1M r1M) (MInterval l2M r2M) =
        do
        NumOrd.maxUpInPlaceEff effortMinmax resLM l1M l2M
        NumOrd.minDnInPlaceEff effortMinmax resRM r1M r2M
        l <- unsafeReadMutable resLM
        r <- unsafeReadMutable resRM
        let (<=?) = NumOrd.pLeqEff effortComp
        case l <=? r of
            Just True -> return True
            _ -> return False


instance 
    (NumOrd.RoundedLatticeEffort e) => 
    (RefOrd.RoundedLatticeEffort (Interval e)) 
    where
    type RefOrd.JoinMeetEffortIndicator (Interval e) = 
        NumOrd.MinmaxEffortIndicator e
    joinmeetDefaultEffort (Interval l r) =
        NumOrd.minmaxDefaultEffort l 

instance 
    (NumOrd.RoundedLattice e) => 
    (RefOrd.RoundedLattice (Interval e)) 
    where
    joinOutEff effort (Interval l1 r1) (Interval l2 r2) =
            Interval l r
            where
            l = NumOrd.maxDnEff effort l1 l2
            r = NumOrd.minUpEff effort r1 r2
    meetOutEff effort (Interval l1 r1) (Interval l2 r2) =
            Interval l r
            where
            l = NumOrd.minDnEff effort l1 l2
            r = NumOrd.maxUpEff effort r1 r2
    joinInEff effort (Interval l1 r1) (Interval l2 r2) =
            Interval l r
            where
            l = NumOrd.maxUpEff effort l1 l2
            r = NumOrd.minDnEff effort r1 r2
    meetInEff effort (Interval l1 r1) (Interval l2 r2) =
            Interval l r
            where
            l = NumOrd.minUpEff effort l1 l2
            r = NumOrd.maxDnEff effort r1 r2

instance
    (NumOrd.RoundedLatticeInPlace e) =>
    (RefOrd.RoundedLatticeInPlace (Interval e))
    where
    joinOutInPlaceEff effort 
            (MInterval resLM resRM) (MInterval l1M r1M) (MInterval l2M r2M) =
        do
        NumOrd.maxDnInPlaceEff effort resLM l1M l2M
        NumOrd.minUpInPlaceEff effort resRM r1M r2M
    meetOutInPlaceEff effort 
            (MInterval resLM resRM) (MInterval l1M r1M) (MInterval l2M r2M) =
        do
        NumOrd.minDnInPlaceEff effort resLM l1M l2M
        NumOrd.maxUpInPlaceEff effort resRM r1M r2M
    joinInInPlaceEff effort 
            (MInterval resLM resRM) (MInterval l1M r1M) (MInterval l2M r2M) =
        do
        NumOrd.maxUpInPlaceEff effort resLM l1M l2M
        NumOrd.minDnInPlaceEff effort resRM r1M r2M
    meetInInPlaceEff effort 
            (MInterval resLM resRM) (MInterval l1M r1M) (MInterval l2M r2M) =
        do
        NumOrd.minUpInPlaceEff effort resLM l1M l2M
        NumOrd.maxDnInPlaceEff effort resRM r1M r2M

instance (NumOrd.ArbitraryOrderedTuple e) => RefOrd.ArbitraryOrderedTuple (Interval e) where
   type RefOrd.Area (Interval e) = NumOrd.Area e
   areaWhole (Interval l r) = NumOrd.areaWhole l
   arbitraryTupleInAreaRelatedBy area = 
       arbitraryIntervalTupleInAreaRefinementRelatedBy (Just area)
   arbitraryTupleRelatedBy = 
       arbitraryIntervalTupleInAreaRefinementRelatedBy Nothing

arbitraryIntervalTupleInAreaRefinementRelatedBy maybeArea indices thinIndices constraints =
    case endpointGens of 
        [] -> Nothing
        _ -> Just $
            do
            gen <- elements endpointGens
            endpointTuple <- gen
            return $ endpointsToIntervals endpointTuple
    where
    endpointGens =
        case maybeArea of
            (Just area) ->
                catMaybes $
                   map (NumOrd.arbitraryTupleInAreaRelatedBy area endpointIndices)
                       endpointConstraintsVersions
            Nothing ->
                catMaybes $
                   map (NumOrd.arbitraryTupleRelatedBy endpointIndices) 
                       endpointConstraintsVersions
    endpointIndices = 
        concat $ map (\ix -> [(ix,-1), (ix,1)]) indices
    endpointsToIntervals [] = []
    endpointsToIntervals (l : r : rest) =
        (Interval l r) : (endpointsToIntervals rest)
    endpointConstraintsVersions =
--        unsafePrintReturn 
--        ("arbitraryIntervalTupleRelatedBy:"
--         ++ "\n indices = " ++ show indices 
--         ++ "\n constraints = " ++ show constraints 
--         ++ "\n endpointIndices = " ++ show endpointIndices 
--         ++ "\n endpointConstraintsVersions = "
--        ) $
        map ((++ thinnessConstraints) . concat) $ 
            combinations $ map intervalConstraintsToEndpointConstraints constraints
    thinnessConstraints = map (\ix -> (((ix,-1),(ix,1)),[EQ])) thinIndices
    intervalConstraintsToEndpointConstraints :: 
        ((ix, ix), [PartialOrdering]) -> [[(((ix,Int), (ix,Int)), [PartialOrdering])]]
    intervalConstraintsToEndpointConstraints ((ix1, ix2),rels) =
        concat $ map forEachRel rels
        where
        endpoints1Comparable = [(((ix1,-1),(ix1, 1)), [EQ,LT,LEE,GT,GEE])]
        endpoints2Comparable = [(((ix2,-1),(ix2, 1)), [EQ,LT,LEE,GT,GEE])]
        endpointsComparable = endpoints1Comparable ++ endpoints2Comparable
        forEachRel EQ = -- both endpoints agree 
            [[(((ix1,-1),(ix2,-1)), [EQ]), (((ix1,1),(ix2,1)), [EQ])]]
        forEachRel GT =  
            -- the interval ix1 is indide ix2, but not equal
            [
                endpointsComparable ++
                [(((ix1,-1),(ix2,-1)), [GT, GEE])] ++ 
                [(((ix1,1),(ix2,1)), [EQ, LT, LEE])]
            ]
            ++
            [
                endpointsComparable ++
                [(((ix1,-1),(ix2,-1)), [EQ, GT, GEE])] ++ 
                [(((ix1,1),(ix2,1)), [LT, LEE])]
            ]
        forEachRel LT =  
            -- the interval ix2 is indide ix1, but not equal
            [
                endpointsComparable ++
                [(((ix2,-1),(ix1,-1)), [GT, GEE])] ++ 
                [(((ix2,1),(ix1,1)), [EQ, LT, LEE])]
            ]
            ++
            [
                endpointsComparable ++
                [(((ix2,-1),(ix1,-1)), [EQ, GT, GEE])] ++ 
                [(((ix2,1),(ix1,1)), [LT, LEE])]
            ]
        forEachRel NC =
            -- either some pair of endpoints is NC:
            [ endpointsComparable ++ [(((ix1,side1), (ix2, side2)),[NC])]  
               | side1 <- [-1,1], side2 <- [-1,1]
            ]
            ++
            -- or the interval ix1 is to the left of ix2
            [
                endpointsComparable ++
                [(((ix1,-1),(ix2,-1)), [LT, LEE]), 
                 (((ix1,1),(ix2,1)), [LT,LEE])]
            ]
            ++
            -- or the interval ix2 is to the left of ix1
            [
                endpointsComparable ++
                [(((ix2,-1),(ix1,-1)), [LT, LEE]), 
                 (((ix2,1),(ix1,1)), [LT,LEE])]
            ]
        forEachRel _ = []
       
       
    