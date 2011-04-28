{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.OpsImplicitEffort
    Description :  convenience binary infix operators with implicit effort parameters  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience binary infix operators with implicit effort parameters.
-}

module Numeric.AERN.Basics.NumericOrder.OpsImplicitEffort where

import Numeric.AERN.Basics.NumericOrder

infix 4 ==?, <==>?, </=>?, <?, <=?, >=?, >?

(==?), (<==>?), (</=>?), (<?), (<=?), (>=?), (>?) :: 
    (PartialComparison t, 
     ?pCompareEffort :: PartialCompareEffortIndicator t) => 
    t -> t -> Maybe Bool

-- | Partial equality
(==?) = pEqualEff ?pCompareEffort
-- | Partial `is comparable to`.
(<==>?) = pComparableEff ?pCompareEffort
-- | Partial `is not comparable to`.
(</=>?) = pIncomparableEff ?pCompareEffort

(<?) = pLessEff ?pCompareEffort
(<=?) = pLeqEff ?pCompareEffort
(>=?) = pGeqEff ?pCompareEffort
(>?) = pGreaterEff ?pCompareEffort

minDn, minUp, maxDn, maxUp ::
    (RoundedLattice t, ?minmaxEffort :: MinmaxEffortIndicator t) =>
    t -> t -> t
minDn = minDnEff ?minmaxEffort
minUp = minUpEff ?minmaxEffort
maxDn = maxDnEff ?minmaxEffort
maxUp = maxUpEff ?minmaxEffort

minOut, maxOut :: 
    (OuterRoundedLattice t, 
     ?minmaxOuterEffort :: MinmaxOuterEffortIndicator t) =>
    t -> t -> t
minOut = minOutEff ?minmaxOuterEffort
maxOut = maxOutEff ?minmaxOuterEffort

minIn, maxIn ::
    (InnerRoundedLattice t,
     ?minmaxInnerEffort :: MinmaxInnerEffortIndicator t) =>
    t -> t -> t
minIn = minInEff ?minmaxInnerEffort
maxIn = maxInEff ?minmaxInnerEffort
