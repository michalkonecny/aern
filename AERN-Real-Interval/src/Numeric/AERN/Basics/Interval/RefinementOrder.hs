{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.Basics.Interval.RefinementOrder
    Description :  interval instances of refinement-ordered structures 
    Copyright   :  (c) Michal Konecny
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
import Numeric.AERN.Basics.Interval.NumericOrder

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

import Numeric.AERN.Misc.List

import Test.QuickCheck

import Data.Maybe

instance 
    (NumOrd.PartialComparison e) => 
    (RefOrd.PartialComparison (Interval e))
    where
    type RefOrd.PartialCompareEffortIndicator (Interval e) = 
        NumOrd.PartialCompareEffortIndicator e 
    pCompareDefaultEffort (Interval l h) = 
        NumOrd.pCompareDefaultEffort l
    pCompareEff effort (Interval l1 h1) (Interval l2 h2) =
            case (c l1 l2, c h1 h2) of
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
    top = Interval NumOrd.highest NumOrd.least
    
instance (NumOrd.HasExtrema e) => (RefOrd.HasBottom (Interval e))
    where
    bottom = Interval NumOrd.least NumOrd.highest

instance (NumOrd.HasExtrema e) => (RefOrd.HasExtrema (Interval e))

instance 
    (NumOrd.RoundedLattice e, NumOrd.PartialComparison e) => 
    (RefOrd.OuterRoundedBasis (Interval e)) 
    where
    type RefOrd.PartialJoinOutEffortIndicator (Interval e) = 
        NumOrd.MinmaxEffortIndicator e 
    partialJoinOutDefaultEffort (Interval l h) =
        NumOrd.minmaxDefaultEffort l
    partialJoinOutEff effort (Interval l1 h1) (Interval l2 h2) = 
            case l <=? h of
                Just True -> Just $ Interval l h
                _ -> Nothing
            where
            (<=?) = NumOrd.pLeqEff (NumOrd.pCompareDefaultEffort l)
            l = NumOrd.maxDnEff effort l1 l2
            h = NumOrd.minUpEff effort h1 h2

instance 
    (NumOrd.RoundedLattice e, NumOrd.PartialComparison e) => 
    (RefOrd.InnerRoundedBasis (Interval e)) 
    where
    type RefOrd.PartialJoinInEffortIndicator (Interval e) = 
        NumOrd.MinmaxEffortIndicator e 
    partialJoinInDefaultEffort (Interval l h) =
        NumOrd.minmaxDefaultEffort l
    partialJoinInEff effort (Interval l1 h1) (Interval l2 h2) = 
            case l <=? h of
                Just True -> Just $ Interval l h
                _ -> Nothing
            where
            (<=?) = NumOrd.pLeqEff (NumOrd.pCompareDefaultEffort l)
            l = NumOrd.maxUpEff effort l1 l2
            h = NumOrd.minDnEff effort h1 h2

instance 
    (NumOrd.RoundedLattice e, NumOrd.PartialComparison e 
--     NumOrd.Lattice (NumOrd.MinmaxEffortIndicator e)
     ) => 
    (RefOrd.RoundedBasis (Interval e)) 


instance 
    (NumOrd.RoundedLatticeEffort e) => 
    (RefOrd.OuterRoundedLatticeEffort (Interval e)) 
    where
    type RefOrd.JoinMeetOutEffortIndicator (Interval e) = 
        NumOrd.MinmaxEffortIndicator e
    joinmeetOutDefaultEffort (Interval l h) =
        NumOrd.minmaxDefaultEffort l 

instance 
    (NumOrd.RoundedLattice e) => 
    (RefOrd.OuterRoundedLattice (Interval e)) 
    where
    joinOutEff effort (Interval l1 h1) (Interval l2 h2) =
            Interval l h
            where
            l = NumOrd.maxDnEff effort l1 l2
            h = NumOrd.minUpEff effort h1 h2
    meetOutEff effort (Interval l1 h1) (Interval l2 h2) =
            Interval l h
            where
            l = NumOrd.minDnEff effort l1 l2
            h = NumOrd.maxUpEff effort h1 h2

instance 
    (NumOrd.RoundedLatticeEffort e) => 
    (RefOrd.InnerRoundedLatticeEffort (Interval e)) 
    where
    type RefOrd.JoinMeetInEffortIndicator (Interval e) = 
        NumOrd.MinmaxEffortIndicator e
    joinmeetInDefaultEffort (Interval l h) =
        NumOrd.minmaxDefaultEffort l 

instance 
    (NumOrd.RoundedLattice e) => 
    (RefOrd.InnerRoundedLattice (Interval e)) 
    where
    joinInEff effort (Interval l1 h1) (Interval l2 h2) =
            Interval l h
            where
            l = NumOrd.maxUpEff effort l1 l2
            h = NumOrd.minDnEff effort h1 h2
    meetInEff effort (Interval l1 h1) (Interval l2 h2) =
            Interval l h
            where
            l = NumOrd.minUpEff effort l1 l2
            h = NumOrd.maxDnEff effort h1 h2

instance 
    (NumOrd.RoundedLattice e, NumOrd.PartialComparison e) => 
    (RefOrd.RoundedLattice (Interval e))

instance (NumOrd.ArbitraryOrderedTuple e) => RefOrd.ArbitraryOrderedTuple (Interval e) where
   type RefOrd.Area (Interval e) = NumOrd.Area e
   areaWhole (Interval l h) = NumOrd.areaWhole l
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
    endpointsToIntervals (l : h : rest) =
        (Interval l h) : (endpointsToIntervals rest)
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
       
       
    