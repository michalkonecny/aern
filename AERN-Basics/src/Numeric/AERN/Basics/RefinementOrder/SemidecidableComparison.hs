{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.ApproxOrder
    Description :  Comparisons with semideciable order  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Comparisons with semideciable order.
    
    This module is hidden and reexported via its parent RefinementOrder. 
-}

module Numeric.AERN.Basics.RefinementOrder.SemidecidableComparison 
where

import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Misc.Maybe
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.RefinementOrder.Extrema
import Numeric.AERN.Basics.Laws.SemidecidableRelation

import Numeric.AERN.Misc.Maybe
import Numeric.AERN.Misc.Bool

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

{-|
    A type with semi-decidable equality and partial order
-}
class SemidecidableComparison t where
    type MaybeCompareEffortIndicator t
    maybeCompareEff :: MaybeCompareEffortIndicator t -> t -> t -> Maybe PartialOrdering
    maybeCompareDefaultEffort :: t -> MaybeCompareEffortIndicator t
    
    maybeCompare :: t -> t -> Maybe PartialOrdering
    maybeCompare a = maybeCompareEff (maybeCompareDefaultEffort a) a

    -- | Semidecidable equality
    (|==?)  :: t -> t -> Maybe Bool
    -- | Semidecidable `is comparable to`.
    (|<==>?)  :: t -> t -> Maybe Bool
    -- | Semidecidable `is not comparable to`.
    (|</=>?)  :: t -> t -> Maybe Bool
    (|<?)     :: t -> t -> Maybe Bool
    (|<=?)    :: t -> t -> Maybe Bool
    (|>=?)    :: t -> t -> Maybe Bool
    (|>?)     :: t -> t -> Maybe Bool

    -- defaults for all convenience operations:
    a |==?   b = fmap (== EQ) (maybeCompare a b)
    a |<?    b = fmap (== LT) (maybeCompare a b)
    a |>?    b = fmap (== GT) (maybeCompare a b)
    a |<==>? b = fmap (/= NC) (maybeCompare a b)
    a |</=>? b = fmap (== NC) (maybeCompare a b)
    a |<=?   b = fmap (`elem` [EQ,LT,LEE]) (maybeCompare a b)
    a |>=?   b = fmap (`elem` [EQ,GT,GEE]) (maybeCompare a b)

-- convenience Unicode math operator notation:
(⊏?) :: (SemidecidableComparison t) => t -> t -> Maybe Bool
(⊏?) = (|<?)
(⊑?) :: (SemidecidableComparison t) => t -> t -> Maybe Bool
(⊑?) = (|<=?)
(⊒?) :: (SemidecidableComparison t) => t -> t -> Maybe Bool
(⊒?) = (|>=?)
(⊐?) :: (SemidecidableComparison t) => t -> t -> Maybe Bool
(⊐?) = (|>?)

propSemidecidableComparisonReflexiveEQ :: 
    (SemidecidableComparison t) => 
    t -> t -> Bool
propSemidecidableComparisonReflexiveEQ _ e = 
    case maybeCompare e e of Just EQ -> True; Nothing -> True; _ -> False 

propSemidecidableComparisonAntiSymmetric :: 
    (SemidecidableComparison t) => 
    t -> t -> t -> Bool
propSemidecidableComparisonAntiSymmetric _ e1 e2 =
    case (maybeCompare e2 e1, maybeCompare e1 e2) of
        (Just b1, Just b2) -> b1 == partialOrderingTranspose b2
        _ -> True 

propSemidecidableComparisonTransitiveEQ :: 
    (SemidecidableComparison t) => 
    t -> t -> t -> t -> Bool
propSemidecidableComparisonTransitiveEQ _ = semidecidableTransitive (|==?)

propSemidecidableComparisonTransitiveLT :: 
    (SemidecidableComparison t) => 
    t -> t -> t -> t -> Bool
propSemidecidableComparisonTransitiveLT _ = semidecidableTransitive (|<?)

propSemidecidableComparisonTransitiveLE :: 
    (SemidecidableComparison t) => 
    t -> t -> t -> t -> Bool
propSemidecidableComparisonTransitiveLE _ = semidecidableTransitive (|<=?)


propExtremaInSemidecidableComparison :: 
    (SemidecidableComparison t, HasExtrema t) => 
    t -> t -> Bool
propExtremaInSemidecidableComparison _ = semidecidableOrderExtrema (|<=?) bottom top

testsSemidecidableComparison :: 
    (SemidecidableComparison t,
     HasExtrema t,
     Arbitrary t, 
     Show t) => 
    (String, t) -> Test
testsSemidecidableComparison (name, sample) =
    testGroup (name ++ " (⊑?)")
        [
         testProperty "anti symmetric" (propSemidecidableComparisonAntiSymmetric sample)
        ,
         testProperty "transitive EQ" (propSemidecidableComparisonTransitiveEQ sample)
        ,
         testProperty "transitive LE" (propSemidecidableComparisonTransitiveLE sample)
        ,
         testProperty "transitive LT" (propSemidecidableComparisonTransitiveLT sample)
        ,
         testProperty "extrema" (propExtremaInSemidecidableComparison sample)
        ]
