{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.Basics.CInterval.NumericOrder
    Description :  refinement-ordered operations for any CInterval instance 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Refinement-ordered operations for any 'CInterval' instance.
-}
module Numeric.AERN.Basics.CInterval.RefinementOrder where

import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.CInterval
import Numeric.AERN.Basics.CInterval.NumericOrder
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

import Numeric.AERN.Misc.List
import Numeric.AERN.Misc.Debug

import Test.QuickCheck

import Data.Maybe

maybeCompareDefaultEffortIntervalRef ::
        (CInterval i, NumOrd.SemidecidableComparison (Endpoint i)) => 
        i -> [EffortIndicator]
maybeCompareDefaultEffortIntervalRef =
    maybeCompareDefaultEffortInterval
        
{-|
  For two intervals, attempt to decide the inclusion partial order.
-}
maybeCompareEffIntervalRef ::
        (CInterval i, NumOrd.SemidecidableComparison (Endpoint i)) => 
        [EffortIndicator] -> i -> i -> Maybe PartialOrdering
maybeCompareEffIntervalRef effort i1 i2 = 
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
    c = NumOrd.maybeCompareEff effort 
    (l1, h1) = getEndpoints i1    
    (l2, h2) = getEndpoints i2

{-|
  For two intervals, decide the inclusion partial order.
-}
compareIntervalRef ::
        (CInterval i, NumOrd.Comparison (Endpoint i)) => 
        i -> i -> PartialOrdering
compareIntervalRef i1 i2 =
    case maybeCompareEffIntervalRef effort i1 i2 of
        Just r -> r 
    where
    effort = maybeCompareDefaultEffortIntervalRef i1

    
bottomInterval ::
     (CInterval i, NumOrd.HasExtrema (Endpoint i)) => i
bottomInterval = fromEndpoints (NumOrd.least, NumOrd.highest)

topInterval ::
     (CInterval i, NumOrd.HasExtrema (Endpoint i)) => i
topInterval = fromEndpoints (NumOrd.highest, NumOrd.least)

basisJoinInterval :: 
    (CInterval i, NumOrd.Lattice (Endpoint i), 
     NumOrd.SemidecidableComparison (Endpoint i)) => 
    i -> i -> Maybe i
basisJoinInterval i1 i2 =
    case l NumOrd.<=? h of
        Just True -> Just $ fromEndpoints (l,h)
        _ -> Nothing
    where
    l = l1 `NumOrd.max` l2
    h = h1 `NumOrd.min` h2
    (l1, h1) = getEndpoints i1    
    (l2, h2) = getEndpoints i2
    
arbitraryIntervalTupleRefinementRelatedBy :: 
    (Ord ix, Show ix, CInterval i, NumOrd.ArbitraryOrderedTuple (Endpoint i)) =>
    [ix] {-^ how many elements should be generated and with what names -} -> 
    [((ix, ix),[PartialOrdering])]
       {-^ required orderings for some pairs of elements -} -> 
    Maybe (Gen [i]) {-^ generator for tuples if the requirements make sense -}   
arbitraryIntervalTupleRefinementRelatedBy indices constraints =
    case endpointGens of 
        [] -> Nothing
        _ -> Just $
            do
            gen <- elements endpointGens
            endpointTuple <- gen
            return $ endpointsToIntervals endpointTuple
    where
    endpointGens = 
        catMaybes $
           map (NumOrd.arbitraryTupleRelatedBy endpointIndices) endpointConstraintsVersions
    endpointIndices = 
        concat $ map (\ix -> [(ix,-1), (ix,1)]) indices
    endpointsToIntervals [] = []
    endpointsToIntervals (l : h : rest) =
        (fromEndpoints (l,h)) : (endpointsToIntervals rest)
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
