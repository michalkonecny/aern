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

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Misc.Maybe
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.RefinementOrder.Extrema
import Numeric.AERN.Basics.Laws.SemidecidableRelation

import Numeric.AERN.Misc.Maybe
import Numeric.AERN.Misc.Bool

import Prelude hiding (EQ, LT, GT)


{-|
    A type with semi-decidable equality and partial order
-}
class SemidecidableComparison t where
    maybeCompareEff :: [EffortIndicator] -> t -> t -> Maybe PartialOrdering
    maybeCompareDefaultEffort :: t -> [EffortIndicator]
    
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

propSemidecidableComparisonReflexiveEQ :: (SemidecidableComparison t) => t -> Bool
propSemidecidableComparisonReflexiveEQ e = 
    case maybeCompare e e of Just EQ -> True; Nothing -> True; _ -> False 

propSemidecidableComparisonAntiSymmetric :: (SemidecidableComparison t) => t -> t -> Bool
propSemidecidableComparisonAntiSymmetric e1 e2 =
    case (maybeCompare e2 e1, maybeCompare e1 e2) of
        (Just b1, Just b2) -> b1 == partialOrderingTranspose b2
        _ -> True 

propSemidecidableComparisonTransitiveEQ :: (SemidecidableComparison t) => t -> t -> t -> Bool
propSemidecidableComparisonTransitiveEQ = semidecidableTransitive (|==?)

propSemidecidableComparisonTransitiveLT :: (SemidecidableComparison t) => t -> t -> t -> Bool
propSemidecidableComparisonTransitiveLT = semidecidableTransitive (|<?)

propSemidecidableComparisonTransitiveLE :: (SemidecidableComparison t) => t -> t -> t -> Bool
propSemidecidableComparisonTransitiveLE = semidecidableTransitive (|<=?)


propExtremaInSemidecidableComparison :: (SemidecidableComparison t, HasExtrema t) => t -> Bool
propExtremaInSemidecidableComparison = semidecidableOrderExtrema (|<=?) bottom top
