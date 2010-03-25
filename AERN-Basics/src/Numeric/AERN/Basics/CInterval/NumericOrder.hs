{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.Basics.CInterval.NumericOrder
    Description :  numeric-ordered operations for any CInterval instance 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Numeric-ordered operations for any 'CInterval' instance.
-}
module Numeric.AERN.Basics.CInterval.NumericOrder where

import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.CInterval
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Numeric.AERN.Misc.List
import Numeric.AERN.Misc.Debug

import Test.QuickCheck

import Data.Maybe

{-|
    Default default effort indicators for numerically comparing 'CInterval' types. 
-}
maybeCompareDefaultEffortInterval ::
        (CInterval i, NumOrd.SemidecidableComparison (Endpoint i)) => 
        i -> [EffortIndicator]
maybeCompareDefaultEffortInterval i =
    zipWith Prelude.max
        (NumOrd.maybeCompareDefaultEffort l)
        (NumOrd.maybeCompareDefaultEffort h)
    where
    (l,h) = getEndpoints i
        
{-|
    Default numerical comparison for 'CInterval' types.
-}
maybeCompareEffInterval ::
        (CInterval i, NumOrd.SemidecidableComparison (Endpoint i)) => 
        [EffortIndicator] -> i -> i -> Maybe PartialOrdering
maybeCompareEffInterval effort i1 i2 = 
    case (c l1 l2, c l1 h2, c h1 l2, c h1 h2) of
        (Just EQ, Just EQ, Just EQ, _) -> Just EQ
        (Just LT, Just LT, Just LT, Just LT) -> Just LT  
        (Just GT, Just GT, Just GT, Just GT) -> Just GT
        (Just relLL, Just relLH, Just EQ, Just relHH) --touching but not all equal
            | relLL `elem` [LT, LEE, EQ] &&  
              relHH `elem` [LT, LEE, EQ] && 
              relLH `elem` [LT, LEE]
              -> Just LEE
        (Just relLL, Just EQ, Just relHL, Just relHH) --touching but not all equal 
            | relLL `elem` [GT, GEE, EQ] &&  
              relHH `elem` [GT, GEE, EQ] && 
              relHL `elem` [GT, GEE] 
              -> Just GEE
        (Just NC, _, _, _) -> Just NC  
        (_, Just NC, _, _) -> Just NC  
        (_, _, Just NC, _) -> Just NC  
        (_, _, _, Just NC) -> Just NC  
        _ -> Nothing
--        _ -> Just NC -- if we want this to be a decidable order
    where
    c = NumOrd.maybeCompareEff effort 
    (l1, h1) = getEndpoints i1    
    (l2, h2) = getEndpoints i2

{-|
    Default binary minimum for 'CInterval' types.
-}
minInterval ::
    (CInterval i, NumOrd.Lattice (Endpoint i)) =>
    i -> i -> i
minInterval i1 i2 =
    fromEndpoints (NumOrd.min l1 l2, NumOrd.min h1 h2)
    where
    (l1, h1) = getEndpoints i1
    (l2, h2) = getEndpoints i2

{-|
    Default binary maximum for 'CInterval' types.
-}
maxInterval ::
    (CInterval i, NumOrd.Lattice (Endpoint i)) =>
    i -> i -> i
maxInterval i1 i2 =
    fromEndpoints (NumOrd.max l1 l2, NumOrd.max h1 h2)
    where
    (l1, h1) = getEndpoints i1
    (l2, h2) = getEndpoints i2

{-|
    Default binary outer-roudned maximum for 'CInterval' types.
-}
maxOuterInterval ::
    (CInterval i, NumOrd.RoundedLattice (Endpoint i)) =>
    [EffortIndicator] -> i -> i -> i
maxOuterInterval effort i1 i2 =
    fromEndpoints (NumOrd.maxDnEff effort l1 l2, NumOrd.maxUpEff effort h1 h2)
    where
    (l1, h1) = getEndpoints i1
    (l2, h2) = getEndpoints i2

{-|
    Default binary inner-roudned maximum for 'CInterval' types.
-}
maxInnerInterval ::
    (CInterval i, NumOrd.RoundedLattice (Endpoint i)) =>
    [EffortIndicator] -> i -> i -> i
maxInnerInterval effort i1 i2 =
    fromEndpoints (NumOrd.maxUpEff effort l1 l2, NumOrd.maxDnEff effort h1 h2)
    where
    (l1, h1) = getEndpoints i1
    (l2, h2) = getEndpoints i2

{-|
    Default binary outer-roudned minimum for 'CInterval' types.
-}
minOuterInterval ::
    (CInterval i, NumOrd.RoundedLattice (Endpoint i)) =>
    [EffortIndicator] -> i -> i -> i
minOuterInterval effort i1 i2 =
    fromEndpoints (NumOrd.minDnEff effort l1 l2, NumOrd.minUpEff effort h1 h2)
    where
    (l1, h1) = getEndpoints i1
    (l2, h2) = getEndpoints i2

{-|
    Default binary inner-roudned minimum for 'CInterval' types.
-}
minInnerInterval ::
    (CInterval i, NumOrd.RoundedLattice (Endpoint i)) =>
    [EffortIndicator] -> i -> i -> i
minInnerInterval effort i1 i2 =
    fromEndpoints (NumOrd.minUpEff effort l1 l2, NumOrd.minDnEff effort h1 h2)
    where
    (l1, h1) = getEndpoints i1
    (l2, h2) = getEndpoints i2

{-|
    Default default effort indicators for numerically comparing 'CInterval' types. 
-}
minmaxDefaultEffortInterval ::
        (CInterval i, NumOrd.RoundedLattice (Endpoint i)) => 
        i -> [EffortIndicator]
minmaxDefaultEffortInterval i =
    zipWith Prelude.max
        (NumOrd.minmaxDefaultEffort l)
        (NumOrd.minmaxDefaultEffort h)
    where
    (l,h) = getEndpoints i

leastInterval ::
     (CInterval i, NumOrd.HasLeast (Endpoint i)) => i
leastInterval = fromEndpoints (NumOrd.least, NumOrd.least)
    
highestInterval ::
    (CInterval i, NumOrd.HasHighest (Endpoint i)) => i
highestInterval = fromEndpoints (NumOrd.highest, NumOrd.highest)

{-|
     A function that can serve as a default implementation of
     the 'NumOrd.ArbitraryOrderedTuple' class for 'CInterval' types.
-}
arbitraryIntervalTupleNumericallyRelatedBy :: 
    (Ord ix, Show ix, CInterval i, NumOrd.ArbitraryOrderedTuple (Endpoint i)) =>
    [ix] {-^ how many elements should be generated and with what names -} -> 
    [((ix, ix),[PartialOrdering])]
       {-^ required orderings for some pairs of elements -} -> 
    Maybe (Gen [i]) {-^ generator for tuples if the requirements make sense -}   
arbitraryIntervalTupleNumericallyRelatedBy indices constraints =
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
        forEachRel EQ = -- both must be thin and equal 
            [[(((ix1,-1),(ix1,1)), [EQ]), (((ix1,1),(ix2,1)), [EQ]), (((ix2,-1),(ix2,1)), [EQ])]]
            ++ -- some cases where the order is not decided:
            -- or the interval ix1 is indide ix2 + ix1 does not coincide with ix2's endpoint
            [
                endpointsComparable ++
                [(((ix1,-1),(ix2,-1)), [EQ, GT, GEE])] ++ 
                [(((ix1,1),(ix2,-1)), [GT, GEE])] ++ 
                [(((ix1,-1),(ix2,1)), [LT, LEE])] ++
                [(((ix1,1),(ix2,1)), [EQ, LT, LEE])]
            ]
            ++
            -- or the interval ix2 is indide ix1 + ix2 does not coincide with ix1's endpoint
            [
                endpointsComparable ++
                [(((ix2,-1),(ix1,-1)), [EQ, GT, GEE])] ++ 
                [(((ix2,1),(ix1,-1)), [GT, GEE])] ++ 
                [(((ix2,-1),(ix1,1)), [LT, LEE])] ++
                [(((ix2,1),(ix1,1)), [EQ, LT, LEE])]
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
                [(((ix1,-1),(ix2,-1)), [EQ, LT, LEE]), 
                 (((ix1,1),(ix2,1)), [EQ,LT,LEE]),
                 (((ix1,-1),(ix2,1)), [LT, LEE]),
                 (((ix1,1),(ix2,-1)), [GT, GEE])]
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
                [(((ix2,-1),(ix1,-1)), [EQ, LT, LEE]), 
                 (((ix2,1),(ix1,1)), [EQ,LT,LEE]),
                 (((ix2,-1),(ix1,1)), [LT, LEE]),
                 (((ix2,1),(ix1,-1)), [GT, GEE])]
            ]
        forEachRel LEE =
            [
                endpointsComparable ++
                [(((ix1,1),(ix2,-1)), [EQ]), 
                 (((ix1,1),(ix2,1)), [LT,LEE,EQ]), (((ix1,-1),(ix2,-1)), [LT,LEE,EQ]),
                 (((ix1,-1),(ix2,1)), [LT,LEE])]
            ]
        forEachRel GEE =
            [
                endpointsComparable ++
                [(((ix1,-1),(ix2,1)), [EQ]), 
                 (((ix1,1),(ix2,1)), [GT,GEE,EQ]), (((ix1,-1),(ix2,-1)), [GT,GEE,EQ]),
                 (((ix1,1),(ix2,-1)), [GT,GEE])]
            ]
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
            