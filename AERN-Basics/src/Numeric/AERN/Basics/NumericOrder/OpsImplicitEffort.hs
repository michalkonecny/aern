{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.OpsImplicitEffort
    Description :  convenience binary symbols with implicit effort parameters  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience binary symbols with implicit effort parameters.
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
