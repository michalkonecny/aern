{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.NumericOrder.OpsImplicitEffort
    Description :  convenience binary infix operators with implicit effort parameters  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience binary infix operators with implicit effort parameters.
-}

module Numeric.AERN.NumericOrder.OpsImplicitEffort where

import Numeric.AERN.NumericOrder

infix 4 ==?, <==>?, </=>?, <?, <=?, >=?, >?

-- | Partial equality
(==?) :: 
    (PartialComparison t, 
     ?pCompareEffort :: PartialCompareEffortIndicator t) => 
    t -> t -> Maybe Bool
(==?) = pEqualEff ?pCompareEffort

-- | Partial `is comparable to`.
(<==>?) :: 
    (PartialComparison t, 
     ?pCompareEffort :: PartialCompareEffortIndicator t) => 
    t -> t -> Maybe Bool 
(<==>?) = pComparableEff ?pCompareEffort

-- | Partial `is not comparable to`.
(</=>?) :: 
    (PartialComparison t, 
     ?pCompareEffort :: PartialCompareEffortIndicator t) => 
    t -> t -> Maybe Bool 
(</=>?) = pIncomparableEff ?pCompareEffort

-- | Partial `strictly less than`
(<?) :: 
    (PartialComparison t, 
     ?pCompareEffort :: PartialCompareEffortIndicator t) => 
    t -> t -> Maybe Bool 
(<?) = pLessEff ?pCompareEffort

-- | Partial `less than or equal to`
(<=?) :: 
    (PartialComparison t, 
     ?pCompareEffort :: PartialCompareEffortIndicator t) => 
    t -> t -> Maybe Bool 
(<=?) = pLeqEff ?pCompareEffort

-- | Partial `greater than or equal to`
(>=?) :: 
    (PartialComparison t, 
     ?pCompareEffort :: PartialCompareEffortIndicator t) => 
    t -> t -> Maybe Bool 
(>=?) = pGeqEff ?pCompareEffort

-- | Partial `strictly greater than`
(>?) :: 
    (PartialComparison t, 
     ?pCompareEffort :: PartialCompareEffortIndicator t) => 
    t -> t -> Maybe Bool
(>?) = pGreaterEff ?pCompareEffort

-- | Downward rounded minimum
minDn :: 
    (RoundedLattice t, ?minmaxEffort :: MinmaxEffortIndicator t) =>
    t -> t -> t
minDn = minDnEff ?minmaxEffort

-- | Upward rounded minimum
minUp ::
    (RoundedLattice t, ?minmaxEffort :: MinmaxEffortIndicator t) =>
    t -> t -> t
minUp = minUpEff ?minmaxEffort

-- | Downward rounded maximum
maxDn ::
    (RoundedLattice t, ?minmaxEffort :: MinmaxEffortIndicator t) =>
    t -> t -> t
maxDn = maxDnEff ?minmaxEffort

-- | Upward rounded maximum
maxUp ::
    (RoundedLattice t, ?minmaxEffort :: MinmaxEffortIndicator t) =>
    t -> t -> t
maxUp = maxUpEff ?minmaxEffort

-- | Outward rounded minimum
minOut :: 
    (RefinementRoundedLattice t, 
     ?minmaxInOutEffort :: MinmaxInOutEffortIndicator t) =>
    t -> t -> t
minOut = minOutEff ?minmaxInOutEffort

-- | Outward rounded maximum
maxOut :: 
    (RefinementRoundedLattice t, 
     ?minmaxInOutEffort :: MinmaxInOutEffortIndicator t) =>
    t -> t -> t
maxOut = maxOutEff ?minmaxInOutEffort

-- | Inward rounded minimum
minIn ::
    (RefinementRoundedLattice t,
     ?minmaxInOutEffort :: MinmaxInOutEffortIndicator t) =>
    t -> t -> t
minIn = minInEff ?minmaxInOutEffort

-- | Outward rounded maximum
maxIn ::
    (RefinementRoundedLattice t,
     ?minmaxInOutEffort :: MinmaxInOutEffortIndicator t) =>
    t -> t -> t
maxIn = maxInEff ?minmaxInOutEffort
