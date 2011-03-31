{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.OpsImplicitEffort
    Description :  convenience binary infix operators with default effort parameters  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience binary infix operators with default effort parameters.
-}

module Numeric.AERN.Basics.NumericOrder.OpsDefaultEffort where

import Numeric.AERN.Basics.NumericOrder

infix 4 ==?, <==>?, </=>?, <?, <=?, >=?, >?

(==?), (<==>?), (</=>?), (<?), (>?), (<=?), (>=?) ::
    (PartialComparison t) => 
    t -> t -> Maybe Bool

-- | Partial equality
(==?) a = pEqualEff (pCompareDefaultEffort a) a
-- | Partial `is comparable to`.
(<==>?) a = pComparableEff (pCompareDefaultEffort a) a
-- | Partial `is not comparable to`.
(</=>?) a = pIncomparableEff (pCompareDefaultEffort a) a

(<?) a = pLessEff (pCompareDefaultEffort a) a
(<=?) a = pLeqEff (pCompareDefaultEffort a) a
(>=?) a = pGeqEff (pCompareDefaultEffort a) a
(>?) a = pGreaterEff (pCompareDefaultEffort a) a

minDn, minUp, maxDn, maxUp ::
    (RoundedLattice t) =>
    t -> t -> t
minDn a = minDnEff (minmaxDefaultEffort a) a
minUp a = minUpEff (minmaxDefaultEffort a) a
maxDn a = maxDnEff (minmaxDefaultEffort a) a
maxUp a = maxUpEff (minmaxDefaultEffort a) a

minOut, maxOut :: 
    (OuterRoundedLattice t) =>
    t -> t -> t
minOut a = minOuterEff (minmaxOuterDefaultEffort a) a
maxOut a = maxOuterEff (minmaxOuterDefaultEffort a) a

minIn, maxIn ::
    (InnerRoundedLattice t) =>
    t -> t -> t
minIn a = minInnerEff (minmaxInnerDefaultEffort a) a
maxIn a = maxInnerEff (minmaxInnerDefaultEffort a) a

