{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.OpsImplicitEffort
    Description :  convenience binary symbols with default effort parameters  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience binary symbols with default effort parameters.
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

      

