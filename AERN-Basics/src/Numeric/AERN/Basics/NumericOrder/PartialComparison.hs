{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.ApproxOrder
    Description :  Comparisons in a semidecidable order  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Comparisons in a semidecidable order.
    
    This module is hidden and reexported via its parent NumericOrder. 
-}

module Numeric.AERN.Basics.NumericOrder.PartialComparison 
where

import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.NumericOrder.Extrema
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
    
    pCompare :: t -> t -> Maybe PartialOrdering
    pCompare a = pCompareEff (pCompareDefaultEffort a) a
    
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
    
    -- | Partial equality
    (==?)    :: t -> t -> Maybe Bool
    -- | Partial `is comparable to`.
    (<==>?)  :: t -> t -> Maybe Bool
    -- | Partial `is not comparable to`.
    (</=>?)  :: t -> t -> Maybe Bool
    (<?)     :: t -> t -> Maybe Bool
    (<=?)    :: t -> t -> Maybe Bool
    (>=?)    :: t -> t -> Maybe Bool
    (>?)     :: t -> t -> Maybe Bool

    -- defaults for all convenience operations:
    pEqualEff effort a b = fmap (== EQ) (pCompareEff effort a b)
    pLessEff effort a b = fmap (== LT) (pCompareEff effort a b)
    pGreaterEff effort a b = fmap (== GT) (pCompareEff effort a b)
    pComparableEff effort a b = fmap (/= NC) (pCompareEff effort a b)
    pIncomparableEff effort a b = fmap (== NC) (pCompareEff effort a b)
    pLeqEff effort a b = fmap (`elem` [EQ,LT,LEE]) (pCompareEff effort a b)
    pGeqEff effort a b = fmap (`elem` [EQ,GT,GEE]) (pCompareEff effort a b)

    a ==?   b = fmap (== EQ) (pCompare a b)
    a <?    b = fmap (== LT) (pCompare a b)
    a >?    b = fmap (== GT) (pCompare a b)
    a <==>? b = fmap (/= NC) (pCompare a b)
    a </=>? b = fmap (== NC) (pCompare a b)
    a <=?   b = fmap (`elem` [EQ,LT,LEE]) (pCompare a b)
    a >=?   b = fmap (`elem` [EQ,GT,GEE]) (pCompare a b)

instance PartialComparison Int where
    type PartialCompareEffortIndicator Int = ()
    pCompareEff = pComparePreludeCompare    
    pCompareDefaultEffort _ = ()
    
pComparePreludeCompare _ a b =
    Just $ toPartialOrdering $ Prelude.compare a b

propPartialComparisonReflexiveEQ :: 
    (PartialComparison t) => 
    t -> t -> Bool
propPartialComparisonReflexiveEQ _ e = 
    case pCompare e e of Just EQ -> True; Nothing -> True; _ -> False 

propPartialComparisonAntiSymmetric :: 
    (PartialComparison t) => 
    t -> t -> t -> Bool
propPartialComparisonAntiSymmetric _ e1 e2 =
    case (pCompare e2 e1, pCompare e1 e2) of
        (Just b1, Just b2) -> b1 == partialOrderingTranspose b2
        _ -> True 

propPartialComparisonTransitiveEQ :: 
    (PartialComparison t) => 
    t -> t -> t -> t -> Bool
propPartialComparisonTransitiveEQ _ = partialTransitive (==?)

propPartialComparisonTransitiveLT :: 
    (PartialComparison t) => 
    t -> t -> t -> t -> Bool
propPartialComparisonTransitiveLT _ = partialTransitive (<?)

propPartialComparisonTransitiveLE :: 
    (PartialComparison t) => 
    t -> t -> t -> t -> Bool
propPartialComparisonTransitiveLE _ = partialTransitive (<=?)

propExtremaInPartialComparison :: 
    (PartialComparison t, HasExtrema t) => 
    t -> t -> Bool
propExtremaInPartialComparison _ = partialOrderExtrema (<=?) least highest

testsPartialComparison :: 
    (PartialComparison t,
     HasExtrema t,
     Arbitrary t, 
     Show t) => 
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
        
