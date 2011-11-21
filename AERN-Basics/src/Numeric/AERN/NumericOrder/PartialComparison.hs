{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.NumericOrder.ApproxOrder
    Description :  Comparisons in a semidecidable order  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Comparisons in a semidecidable order.
    
    This module is hidden and reexported via its parent NumericOrder. 
-}

module Numeric.AERN.NumericOrder.PartialComparison 
where

import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.NumericOrder.Extrema
import Numeric.AERN.NumericOrder.Arbitrary

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.Laws.PartialRelation

import Numeric.AERN.Misc.Maybe
import Numeric.AERN.Misc.Bool

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

{-|
    A type with semi-decidable equality and partial order
-}
class PartialComparison t where
    type PartialCompareEffortIndicator t
    pCompareEff :: PartialCompareEffortIndicator t -> t -> t -> Maybe PartialOrdering
    pCompareDefaultEffort :: t -> PartialCompareEffortIndicator t
    
    -- | Partial equality
    pEqualEff :: (PartialCompareEffortIndicator t) -> t -> t -> Maybe Bool
    -- | Partial `is comparable to`.
    pComparableEff :: (PartialCompareEffortIndicator t) -> t -> t -> Maybe Bool
    -- | Partial `is not comparable to`.
    pIncomparableEff :: (PartialCompareEffortIndicator t) -> t -> t -> Maybe Bool
    pLessEff :: (PartialCompareEffortIndicator t) -> t -> t -> Maybe Bool
    pLeqEff :: (PartialCompareEffortIndicator t) -> t -> t -> Maybe Bool
    pGeqEff :: (PartialCompareEffortIndicator t) -> t -> t -> Maybe Bool
    pGreaterEff :: (PartialCompareEffortIndicator t) -> t -> t -> Maybe Bool
    
    -- defaults for all convenience operations:
    pEqualEff effort a b =
        case pCompareEff effort a b of
            Just EQ -> Just True
            Just LEE -> Nothing
            Just GEE -> Nothing
            Just _ -> Just False
            _ -> Nothing
    pLessEff effort a b = 
        case pCompareEff effort a b of
            Just LT -> Just True
            Just LEE -> Nothing
            Just _ -> Just False
            _ -> Nothing
    pGreaterEff effort a b = 
        case pCompareEff effort a b of
            Just GT -> Just True
            Just GEE -> Nothing
            Just _ -> Just False
            _ -> Nothing
    pLeqEff effort a b =
        case pCompareEff effort a b of
            Just EQ -> Just True
            Just LT -> Just True
            Just LEE -> Just True
            Just GEE -> Nothing
            Just _ -> Just False
            _ -> Nothing
    pGeqEff effort a b =
        case pCompareEff effort a b of
            Just EQ -> Just True
            Just GT -> Just True
            Just GEE -> Just True
            Just LEE -> Nothing
            Just _ -> Just False
            _ -> Nothing
    pComparableEff effort a b = fmap (/= NC) (pCompareEff effort a b)
    pIncomparableEff effort a b = fmap (== NC) (pCompareEff effort a b)


instance PartialComparison Int where
    type PartialCompareEffortIndicator Int = ()
    pCompareDefaultEffort _ = ()
    pCompareEff = pComparePreludeCompare    
    
instance PartialComparison Integer where
    type PartialCompareEffortIndicator Integer = ()
    pCompareDefaultEffort _ = ()
    pCompareEff = pComparePreludeCompare    
    
instance PartialComparison Rational where
    type PartialCompareEffortIndicator Rational = ()
    pCompareDefaultEffort _ = ()
    pCompareEff = pComparePreludeCompare    

instance PartialComparison Double where
    type PartialCompareEffortIndicator Double = ()
    pCompareEff _ a b = Just $ toPartialOrdering $ Prelude.compare a b
--        case (isNaN a, isNaN b) of
--           (False, False) -> Just $ toPartialOrdering $ Prelude.compare a b  
--           (True, True) -> Just EQ
--           _ -> Just NC 
    pCompareDefaultEffort _ = ()
    
pComparePreludeCompare _ a b =
    Just $ toPartialOrdering $ Prelude.compare a b

propPartialComparisonReflexiveEQ :: 
    (PartialComparison t) => 
    t -> 
    (PartialCompareEffortIndicator t) -> 
    (UniformlyOrderedSingleton t) -> 
    Bool
propPartialComparisonReflexiveEQ _ effort (UniformlyOrderedSingleton e) = 
    case pCompareEff effort e e of Just EQ -> True; Nothing -> True; _ -> False 

propPartialComparisonAntiSymmetric :: 
    (PartialComparison t) => 
    t -> 
    (PartialCompareEffortIndicator t) -> 
    UniformlyOrderedPair t -> 
    Bool
propPartialComparisonAntiSymmetric _ effort (UniformlyOrderedPair (e1,e2)) =
    case (pCompareEff effort e2 e1, pCompareEff effort e1 e2) of
        (Just b1, Just b2) -> b1 == partialOrderingTranspose b2
        _ -> True 

propPartialComparisonTransitiveEQ :: 
    (PartialComparison t) => 
    t -> 
    (PartialCompareEffortIndicator t) -> 
    UniformlyOrderedTriple t -> 
    Bool
propPartialComparisonTransitiveEQ _ effort 
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    partialTransitive (pEqualEff effort) e1 e2 e3

propPartialComparisonTransitiveLT :: 
    (PartialComparison t) => 
    t -> 
    (PartialCompareEffortIndicator t) -> 
    UniformlyOrderedTriple t -> 
    Bool
propPartialComparisonTransitiveLT _ effort 
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    partialTransitive (pLessEff effort) e1 e2 e3

propPartialComparisonTransitiveLE :: 
    (PartialComparison t) => 
    t -> 
    (PartialCompareEffortIndicator t) -> 
    UniformlyOrderedTriple t -> 
    Bool
propPartialComparisonTransitiveLE _ effort
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    partialTransitive (pLeqEff effort) e1 e2 e3

propExtremaInPartialComparison :: 
    (PartialComparison t, HasExtrema t) => 
    t -> 
    (PartialCompareEffortIndicator t) -> 
    UniformlyOrderedSingleton t -> 
    Bool
propExtremaInPartialComparison _ effort 
        (UniformlyOrderedSingleton e) = 
    partialOrderExtrema (pLeqEff effort) least greatest e

testsPartialComparison :: 
    (PartialComparison t,
     HasExtrema t,
     ArbitraryOrderedTuple t, Show t,
     Arbitrary (PartialCompareEffortIndicator t),
     Show (PartialCompareEffortIndicator t)) => 
    (String, t) -> Test
testsPartialComparison (name, sample) =
    testGroup (name ++ " (>=?)")
        [
         testProperty "anti symmetric" (propPartialComparisonAntiSymmetric sample)
        ,
         testProperty "transitive EQ" (propPartialComparisonTransitiveEQ sample)
        ,
         testProperty "transitive LE" (propPartialComparisonTransitiveLE sample)
        ,
         testProperty "transitive LT" (propPartialComparisonTransitiveLT sample)
        ,
         testProperty "extrema" (propExtremaInPartialComparison sample)
        ]
        
