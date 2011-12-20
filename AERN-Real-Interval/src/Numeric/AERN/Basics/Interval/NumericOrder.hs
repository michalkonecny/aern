{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.Basics.Interval.NumericOrder
    Description :  interval instances of numeric-ordered structures 
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Interval instances of numeric-ordered structures.
    
    This is a hidden module reexported via its parent.
-}
module Numeric.AERN.Basics.Interval.NumericOrder where

import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.Basics.Consistency
import Numeric.AERN.Basics.PartialOrdering

import Numeric.AERN.Basics.Interval.Basics
import Numeric.AERN.Basics.Interval.Consistency
import Numeric.AERN.Basics.Interval.Mutable

import Numeric.AERN.Misc.List

import qualified Numeric.AERN.NumericOrder as NumOrd

import Test.QuickCheck

import Data.Maybe

instance
    (NumOrd.PartialComparison e) => 
    (NumOrd.PartialComparison (Interval e))
    where
    type NumOrd.PartialCompareEffortIndicator (Interval e) = 
        NumOrd.PartialCompareEffortIndicator e 
    pCompareDefaultEffort (Interval l r) = NumOrd.pCompareDefaultEffort l
    pCompareEff effort i1 i2 =
        case partialInfo2PartialOrdering $ NumOrd.pCompareInFullEff effort i1 i2 of
            [ord] -> Just ord
            _ -> Nothing
    pCompareInFullEff effort i1@(Interval l1 r1) i2@(Interval l2 r2) =
        case (isConsistentEff effort i1, isConsistentEff effort i2) of
            (Just True, Just True) ->
                PartialOrderingPartialInfo
                {
                    pOrdInfEQ = fromD (eqD, neqD),
                    pOrdInfLEQ = fromD (leqD, nleqD),
                    pOrdInfLT = fromD (ltD, eqD || gtD || ncD),
                    pOrdInfGT = fromD (gtD, eqD || ltD || ncD),
                    pOrdInfGEQ = fromD (geqD, ngeqD),
                    pOrdInfNC = fromD (ncD, leqD || geqD)
                }
            _ -> 
                partialOrderingPartialInfoAllNothing
        where
        fromD (definitelyTrue, definitelyFalse) =
            case (definitelyTrue, definitelyFalse) of
                (True, _) -> Just True
                (_, True) -> Just False
                _ -> Nothing
        -- answers to the "definitely related" questions:
        eqD = leqD && geqD
        neqD = ltD || gtD || ncD
        ltD = r1 `less` l2 == jt
        leqD = r1 `leq` l2 == jt
        nleqD = l1 `leq` r2 == jf
        gtD = r2 `less` l1 == jt
        geqD = r2 `leq` l1 == jt
        ngeqD = l2 `leq` r1 == jf
        ncD = (r1 `nc` l2 == jt) && (l1 `nc` r2 == jt)
        jt = Just True
        jf = Just False
        leq = NumOrd.pLeqEff effort
        less = NumOrd.pLessEff effort
        nc = NumOrd.pIncomparableEff effort
            
                
instance
    (NumOrd.RoundedLatticeEffort e) =>
    (NumOrd.RefinementRoundedLatticeEffort (Interval e))
    where
    type NumOrd.MinmaxInOutEffortIndicator (Interval e) = NumOrd.MinmaxEffortIndicator e
    minmaxInOutDefaultEffort (Interval l r) = NumOrd.minmaxDefaultEffort l  

instance
    (NumOrd.RoundedLattice e) =>
    (NumOrd.RefinementRoundedLattice (Interval e))
    where
    minOutEff effort (Interval l1 r1) (Interval l2 r2) =
        Interval (NumOrd.minDnEff effort l1 l2) (NumOrd.minUpEff effort r1 r2)
    maxOutEff effort (Interval l1 r1) (Interval l2 r2) =
        Interval (NumOrd.maxDnEff effort l1 l2) (NumOrd.maxUpEff effort r1 r2)
    minInEff effort (Interval l1 r1) (Interval l2 r2) =
        Interval (NumOrd.minUpEff effort l1 l2) (NumOrd.minDnEff effort r1 r2)
    maxInEff effort (Interval l1 r1) (Interval l2 r2) =
        Interval (NumOrd.maxUpEff effort l1 l2) (NumOrd.maxDnEff effort r1 r2)

instance
    (NumOrd.RoundedLatticeInPlace e) =>
    (NumOrd.RefinementRoundedLatticeInPlace (Interval e))
    where
    minOutInPlaceEff effort 
            (MInterval resLM resRM) (MInterval l1M r1M) (MInterval l2M r2M) =
        do
        NumOrd.minDnInPlaceEff effort resLM l1M l2M
        NumOrd.minUpInPlaceEff effort resRM r1M r2M
    maxOutInPlaceEff effort 
            (MInterval resLM resRM) (MInterval l1M r1M) (MInterval l2M r2M) =
        do
        NumOrd.maxDnInPlaceEff effort resLM l1M l2M
        NumOrd.maxUpInPlaceEff effort resRM r1M r2M
    minInInPlaceEff effort 
            (MInterval resLM resRM) (MInterval l1M r1M) (MInterval l2M r2M) =
        do
        NumOrd.minUpInPlaceEff effort resLM l1M l2M
        NumOrd.minDnInPlaceEff effort resRM r1M r2M
    maxInInPlaceEff effort 
            (MInterval resLM resRM) (MInterval l1M r1M) (MInterval l2M r2M) =
        do
        NumOrd.maxUpInPlaceEff effort resLM l1M l2M
        NumOrd.maxDnInPlaceEff effort resRM r1M r2M

instance (NumOrd.HasLeast e) => (NumOrd.HasLeast (Interval e))
    where
    least (Interval sampleE _) = 
        Interval (NumOrd.least sampleE) (NumOrd.least sampleE)
    
instance (NumOrd.HasGreatest e) => (NumOrd.HasGreatest (Interval e))
    where
    greatest (Interval sampleE _) = 
        Interval (NumOrd.greatest sampleE) (NumOrd.greatest sampleE)
    
instance (NumOrd.HasExtrema e) => (NumOrd.HasExtrema (Interval e))

instance (NumOrd.ArbitraryOrderedTuple e) => NumOrd.ArbitraryOrderedTuple (Interval e) where
   type NumOrd.Area (Interval e) = NumOrd.Area e
   areaWhole (Interval l r) = NumOrd.areaWhole l
   arbitraryTupleInAreaRelatedBy area = 
       arbitraryIntervalTupleInAreaNumericallyRelatedBy (Just area)
   arbitraryTupleRelatedBy = 
       arbitraryIntervalTupleInAreaNumericallyRelatedBy Nothing

arbitraryIntervalTupleInAreaNumericallyRelatedBy maybeArea indices constraints =
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
        map concat $ combinations $ map intervalConstraintsToEndpointConstraints constraints
    intervalConstraintsToEndpointConstraints :: 
        ((ix, ix), [PartialOrdering]) -> [[(((ix,Int), (ix,Int)), [PartialOrdering])]]
    intervalConstraintsToEndpointConstraints ((ix1, ix2),rels) =
        concat $ map forEachRel rels
        where
        endpoints1Comparable = [(((ix1,-1),(ix1, 1)), [EQ,LT,GT])]
        endpoints2Comparable = [(((ix2,-1),(ix2, 1)), [EQ,LT,GT])]
        endpointsComparable = endpoints1Comparable ++ endpoints2Comparable
        forEachRel EQ = -- both must be thin and equal 
            [[(((ix1,-1),(ix1,1)), [EQ]), (((ix1,1),(ix2,1)), [EQ]), (((ix2,-1),(ix2,1)), [EQ])]]
            ++ -- some cases where the order is not decided:
            -- or the interval ix1 is indide ix2 + ix1 does not coincide with ix2's endpoint
            [
                endpointsComparable ++
                [(((ix1,-1),(ix2,-1)), [EQ, GT])] ++ 
                [(((ix1,1),(ix2,-1)), [GT])] ++ 
                [(((ix1,-1),(ix2,1)), [LT])] ++
                [(((ix1,1),(ix2,1)), [EQ, LT])]
            ]
            ++
            -- or the interval ix2 is indide ix1 + ix2 does not coincide with ix1's endpoint
            [
                endpointsComparable ++
                [(((ix2,-1),(ix1,-1)), [EQ, GT])] ++ 
                [(((ix2,1),(ix1,-1)), [GT])] ++ 
                [(((ix2,-1),(ix1,1)), [LT])] ++
                [(((ix2,1),(ix1,1)), [EQ, LT])]
            ]
        forEachRel LT = -- both endpoints of ix1 must be less than both endpoints of ix2  
            [
                endpointsComparable ++ 
                [(((ix1,side1),(ix2,side2)), [LT]) | side1 <- [-1,1], side2 <- [-1,1]]
            ]
            ++ -- some undecidable cases:
            -- or the interval ix1 overlaps ix2 and ix1 is slightly to the left of ix2
            [
                endpointsComparable ++
                [(((ix1,-1),(ix2,-1)), [EQ, LT]), 
                 (((ix1,1),(ix2,1)), [EQ,LT]),
                 (((ix1,-1),(ix2,1)), [LT]),
                 (((ix1,1),(ix2,-1)), [GT])]
            ]
        forEachRel GT = -- both endpoints of ix1 must be greater than both endpoints of ix2  
            [
                endpointsComparable ++ 
                [(((ix1,side1),(ix2,side2)), [GT]) | side1 <- [-1,1], side2 <- [-1,1]]
            ]
            ++ -- some undecidable cases:
            -- or the interval ix1 overlaps ix2 and ix1 is slightly to the right of ix2
            [
                endpointsComparable ++
                [(((ix2,-1),(ix1,-1)), [EQ, LT]), 
                 (((ix2,1),(ix1,1)), [EQ,LT]),
                 (((ix2,-1),(ix1,1)), [LT]),
                 (((ix2,1),(ix1,-1)), [GT])]
            ]
--        forEachRel LEE =
--            [
--                endpointsComparable ++
--                [(((ix1,1),(ix2,-1)), [EQ]), 
--                 (((ix1,1),(ix2,1)), [LT,LEE,EQ]), (((ix1,-1),(ix2,-1)), [LT,LEE,EQ]),
--                 (((ix1,-1),(ix2,1)), [LT,LEE])]
--            ]
--        forEachRel GEE =
--            [
--                endpointsComparable ++
--                [(((ix1,-1),(ix2,1)), [EQ]), 
--                 (((ix1,1),(ix2,1)), [GT,GEE,EQ]), (((ix1,-1),(ix2,-1)), [GT,GEE,EQ]),
--                 (((ix1,1),(ix2,-1)), [GT,GEE])]
--            ]
        forEachRel NC =
            -- either some pair of endpoints is NC:
            [ endpointsComparable ++ [(((ix1,side1), (ix2, side2)),[NC])]  
               | side1 <- [-1,1], side2 <- [-1,1]
            ]
--            ++
--            -- or the interval ix1 is indide ix2 + ix1 does not coincide with ix2's endpoint
--            [
--                endpointsComparable ++
--                [(((ix1,-1),(ix2,-1)), [EQ, GT, GEE])] ++ 
--                [(((ix1,1),(ix2,-1)), [GT, GEE])] ++ 
--                [(((ix1,-1),(ix2,1)), [LT, LEE])] ++
--                [(((ix1,1),(ix2,1)), [EQ, LT, LEE])]
--            ]
--            ++
--            -- or the interval ix2 is indide ix1 + ix2 does not coincide with ix1's endpoint
--            [
--                endpointsComparable ++
--                [(((ix2,-1),(ix1,-1)), [EQ, GT, GEE])] ++ 
--                [(((ix2,1),(ix1,-1)), [GT, GEE])] ++ 
--                [(((ix2,-1),(ix1,1)), [LT, LEE])] ++
--                [(((ix2,1),(ix1,1)), [EQ, LT, LEE])]
--            ]
--            ++
--            -- or the interval ix1 overlaps ix2 and ix1 is slightly to the left of ix2
--            [
--                endpointsComparable ++
--                [(((ix1,-1),(ix2,-1)), [EQ, LT, LEE]), 
--                 (((ix1,1),(ix2,1)), [EQ,LT,LEE]),
--                 (((ix1,-1),(ix2,1)), [LT, LEE]),
--                 (((ix1,1),(ix2,-1)), [GT, GEE])]
--            ]
--            ++
--            -- or the interval ix1 overlaps ix2 and ix1 is slightly to the right of ix2
--            [
--                endpointsComparable ++
--                [(((ix2,-1),(ix1,-1)), [EQ, LT, LEE]), 
--                 (((ix2,1),(ix1,1)), [EQ,LT,LEE]),
--                 (((ix2,-1),(ix1,1)), [LT, LEE]),
--                 (((ix2,1),(ix1,-1)), [GT, GEE])]
--            ]
   