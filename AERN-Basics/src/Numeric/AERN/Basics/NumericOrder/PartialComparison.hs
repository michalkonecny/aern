{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
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

infix 4 ==?, <==>?, </=>?, <?, <=?, >=?, >?

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
    
    -- | Partial equality
    (==?)    :: (?pCompareEffort :: PartialCompareEffortIndicator t) => t -> t -> Maybe Bool
    -- | Partial `is comparable to`.
    (<==>?)  :: (?pCompareEffort :: PartialCompareEffortIndicator t) => t -> t -> Maybe Bool
    -- | Partial `is not comparable to`.
    (</=>?)  :: (?pCompareEffort :: PartialCompareEffortIndicator t) => t -> t -> Maybe Bool
    (<?)     :: (?pCompareEffort :: PartialCompareEffortIndicator t) => t -> t -> Maybe Bool
    (<=?)    :: (?pCompareEffort :: PartialCompareEffortIndicator t) => t -> t -> Maybe Bool
    (>=?)    :: (?pCompareEffort :: PartialCompareEffortIndicator t) => t -> t -> Maybe Bool
    (>?)     :: (?pCompareEffort :: PartialCompareEffortIndicator t) => t -> t -> Maybe Bool

    -- defaults for all convenience operations:
    pEqualEff effort a b = fmap (== EQ) (pCompareEff effort a b)
    pLessEff effort a b = fmap (== LT) (pCompareEff effort a b)
    pGreaterEff effort a b = fmap (== GT) (pCompareEff effort a b)
    pComparableEff effort a b = fmap (/= NC) (pCompareEff effort a b)
    pIncomparableEff effort a b = fmap (== NC) (pCompareEff effort a b)
    pLeqEff effort a b = fmap (`elem` [EQ,LT,LEE]) (pCompareEff effort a b)
    pGeqEff effort a b = fmap (`elem` [EQ,GT,GEE]) (pCompareEff effort a b)

    (==?) = pEqualEff ?pCompareEffort
    (<==>?) = pComparableEff ?pCompareEffort
    (</=>?) = pIncomparableEff ?pCompareEffort
    (<?) = pLessEff ?pCompareEffort
    (>?) = pGreaterEff ?pCompareEffort
    (<=?) = pLeqEff ?pCompareEffort
    (>=?) = pGeqEff ?pCompareEffort
      
--    a ==?   b = fmap (== EQ) (pCompare a b)
--    a <?    b = fmap (== LT) (pCompare a b)
--    a >?    b = fmap (== GT) (pCompare a b)
--    a <==>? b = fmap (/= NC) (pCompare a b)
--    a </=>? b = fmap (== NC) (pCompare a b)
--    a <=?   b = fmap (`elem` [EQ,LT,LEE]) (pCompare a b)
--    a >=?   b = fmap (`elem` [EQ,GT,GEE]) (pCompare a b)

instance PartialComparison Int where
    type PartialCompareEffortIndicator Int = ()
    pCompareEff = pComparePreludeCompare    
    pCompareDefaultEffort _ = ()
    
pComparePreludeCompare _ a b =
    Just $ toPartialOrdering $ Prelude.compare a b

propPartialComparisonReflexiveEQ :: 
    (PartialComparison t) => 
    t -> (PartialCompareEffortIndicator t) -> t -> Bool
propPartialComparisonReflexiveEQ _ effort e = 
    case pCompareEff effort e e of Just EQ -> True; Nothing -> True; _ -> False 

propPartialComparisonAntiSymmetric :: 
    (PartialComparison t) => 
    t -> (PartialCompareEffortIndicator t) -> t -> t -> Bool
propPartialComparisonAntiSymmetric _ effort e1 e2 =
    case (pCompareEff effort e2 e1, pCompareEff effort e1 e2) of
        (Just b1, Just b2) -> b1 == partialOrderingTranspose b2
        _ -> True 

propPartialComparisonTransitiveEQ :: 
    (PartialComparison t) => 
    t -> (PartialCompareEffortIndicator t) -> t -> t -> t -> Bool
propPartialComparisonTransitiveEQ _ effort = 
    let ?pCompareEffort = effort in partialTransitive (==?)

propPartialComparisonTransitiveLT :: 
    (PartialComparison t) => 
    t -> (PartialCompareEffortIndicator t) -> t -> t -> t -> Bool
propPartialComparisonTransitiveLT _ effort = 
    let ?pCompareEffort = effort in partialTransitive (<?)

propPartialComparisonTransitiveLE :: 
    (PartialComparison t) => 
    t -> (PartialCompareEffortIndicator t) -> t -> t -> t -> Bool
propPartialComparisonTransitiveLE _ effort =
    let ?pCompareEffort = effort in partialTransitive (<=?)

propExtremaInPartialComparison :: 
    (PartialComparison t, HasExtrema t) => 
    t -> (PartialCompareEffortIndicator t) -> t -> Bool
propExtremaInPartialComparison _ effort = 
    let ?pCompareEffort = effort in partialOrderExtrema (<=?) least highest

testsPartialComparison :: 
    (PartialComparison t,
     HasExtrema t,
     Arbitrary t, Show t,
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
        
